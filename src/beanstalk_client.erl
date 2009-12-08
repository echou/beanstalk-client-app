-module(beanstalk_client).
-author('echou327@gmail.com').

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([start/1, start_link/1, call/3]).

-include("beanstalk.hrl").

-record(state, {role=producer, % or consumer
                proto,
                rq,
                mq=[]
            }).


start(Args) ->
    gen_server:start(?MODULE, [Args], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

% callbacks

init([{Tube, Role, Host, Port}=Arg]) ->
    process_flag(trap_exit, true),

    ?PRINT("init(~p)", [Arg]),

    Proto = beanstalk_proto:new(Host, Port),
    Proto:connect(),

    pg2:create({?MODULE, Tube, Role}),
    pg2:join({?MODULE, Tube, Role}, self()),

    erlang:send_after(100, self(), {init_tube, Tube}),
    
    {ok, #state{proto=Proto, rq=queue:new(), role=Role}}.


handle_info({init_tube, Tube}, #state{role=producer}=State) ->
    {noreply, do_send({use, Tube}, State)};
handle_info({init_tube, Tube}, #state{role=consumer}=State) ->
    NewState = do_send({watch, Tube}, State),
    erlang:send_after(1000, self(), reserve),
    {noreply, NewState};

handle_info({delete, Id}, #state{role=consumer}=State) ->
    {noreply, do_send({delete, Id}, State)};

handle_info(reserve, #state{role=consumer}=State) ->
    {noreply, do_send(reserve, State)};

handle_info({tcp, _Sock, Bin}, #state{proto=Proto}=State) ->
    error_logger:info_msg("[~p] recv:~n\"~s\"~n", [self(), Bin]),
    NewState = lists:foldl(fun process_packet/2, State, Proto:recv(Bin)),
    {noreply, NewState};

handle_info({inet_reply, _Sock, ok}, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    error_logger:info_msg("handle_info: ~p~n", [_Msg]),
    {noreply, State}.


% use/watch results
process_packet({using, _Tube}, #state{role=producer}=State) -> State;
process_packet({watching, _Count}, #state{role=consumer}=State) -> State;

% reserve results
process_packet(deadline_soon, #state{role=consumer}=State) -> State;
process_packet(timed_out, #state{role=consumer}=State) -> State;
process_packet({reserved, Id, Body}, #state{role=consumer, mq=MQ}=State) ->
    self() ! {delete, Id},
    self() ! reserve,
    State#state{mq=[{Id,Body}|MQ]};

% delete/release/bury/touch results
process_packet(not_found, State) -> State;
process_packet(deleted, State) -> State;
process_packet(released, State) -> State;
process_packet(buried, State) -> State;
process_packet(touched, State) -> State;

process_packet(P, #state{rq=Q}=State) ->
    case queue:out(Q) of
        { {value, no_reply}, Q1 } ->
            State#state{rq=Q1};
        { {value, From}, Q1} ->
            gen_server:reply(From, P),
            State#state{rq=Q1};
        _ ->
            State
    end.

handle_call({put, _Data, _Params}=Req, From, #state{role=producer}=State) ->
    {noreply, do_send(Req, From, State)};

handle_call(get, _From, #state{role=consumer, mq=MQ}=State) ->
    {reply, MQ, State#state{mq=[]}};

handle_call(role, _From, #state{role=Role}=State) ->
    {reply, Role, State};

handle_call(_Req, _From, State) ->
    {reply, {error, not_supported}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


do_send(Req, #state{proto=Proto}=State) ->
    Proto:send(Req),
    State.

do_send(Req, From, #state{proto=Proto, rq=Q}=State) ->
    Proto:send(Req),
    State#state{rq=queue:in(From, Q)}.
    
call(Tube, Role, Req) ->
    Pid = pg2:get_closest_pid({?MODULE, Tube, Role}),
    gen_server:call(Pid, Req).

