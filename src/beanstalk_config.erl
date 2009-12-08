-module(beanstalk_config).
-author('echou327@gmail.com').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-record(state, {}).

-define(PRINT(Fmt, Args), error_logger:info_msg("[~p] " ++ Fmt ++ "~n", [?MODULE|Args])).

start_link() ->
    	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?PRINT("init", []),

	process_flag(trap_exit, true),

    Tubes = beanstalk_util:get_app_env(tubes, []),
    ?PRINT("tubes ~p", [Tubes]),
    parse_tubes(Tubes),
	
	{ok, #state{}}.


handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) -> 
	{noreply, State}.

handle_info(_Info, State) -> 
	{noreply, State}.

terminate(Reason, _State) ->
	?PRINT("terminate() reason: ~p", [Reason]).

code_change(_OldVsn, _Extra, State) ->
	State.


% internal functions

parse_tubes(Tubes) ->
    % start beanstalk_clients
    lists:foreach(
        fun({Tube, Servers, ProducerCount, ConsumerCount}) ->
            lists:foreach(
                fun(Server) ->
                    {{A,B,C,D}=Addr,Port} = normalize_addr(Server),
                    ?PRINT("Starting ~p@~p.~p.~p.~p:~p (~p producers, ~p consumers) ...", [Tube,A,B,C,D,Port,ProducerCount,ConsumerCount]),
                    beanstalk_client_sup:start_child(Tube,producer,Addr,Port,ProducerCount),
                    beanstalk_client_sup:start_child(Tube,consumer,Addr,Port,ConsumerCount)
                end,
                Servers)
        end,
        Tubes).

normalize_addr({{_,_,_,_}=Addr, Port}) ->
    {Addr, Port};
normalize_addr({Addr, Port}) when is_list(Addr) ->
    {Addr1, _} = normalize_addr(Addr),
    {Addr1, Port};
normalize_addr(Addr) when is_list(Addr) ->
    case string:tokens(Addr, ":") of
        [IP] ->
            {ok, Addr1} = inet:getaddr(IP, inet),
            {Addr1, 11300};
        [IP,PortStr|_] ->
            {ok, Addr1} = inet:getaddr(IP, inet),
            Port1 = case string:to_integer(PortStr) of
                        {Port, []} -> Port;
                        _ -> 11300
                    end,
            {Addr1, Port1};
        _ ->
            {{127,0,0,1}, 11300}
    end.
