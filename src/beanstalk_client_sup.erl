-module(beanstalk_client_sup).
-author('echou327@gmail.com').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0, start_child/5, restart_child/4, terminate_child/4]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("[~p] Started.~n", [?MODULE]),

    {ok, {{one_for_one, 10, 10}, []}}.

start_child(Tube, Role, Host, Port, Num) ->
    lists:foreach(
        fun(I) ->
            Id = {beanstalk_client, {Tube, Role, Host, Port}, I},
			Spec = {Id,{beanstalk_client,start_link,[{Tube,Role,Host,Port}]},permanent,2000,worker,[beanstalk_client]},
            case supervisor:start_child(?MODULE, Spec) of
				{ok, _} -> ok;
				{ok, _, _} -> ok;
				{error, {already_started,_}} ->
					supervisor:terminate_child(?MODULE, Id),
					supervisor:delete_child(?MODULE, Id),
					supervisor:start_child(?MODULE, Spec);
				{error, already_present} ->
					supervisor:delete_child(?MODULE, Id),
					supervisor:start_child(?MODULE, Spec);
				{error, _Error} ->
					erlang:error(_Error)
			end
        end,
        lists:seq(1, Num)).

terminate_child(Tube, Role, Host, Port) ->
    F = fun({{beanstalk_client, {Tube1, Role1, Host1, Port1}, _Index} = Id, _Child, _Type, _Modules}) when Tube=:=Tube1,Role=:=Role1,Host=:=Host1,Port=:=Port1-> 
				supervisor:terminate_child(?MODULE, Id);
           (_) -> 
				ignore
        end,
    lists:foreach(F, supervisor:which_children(?MODULE)).

restart_child(Tube, Role, Host, Port) ->
    F = fun({{beanstalk_client, {Tube1, Role1, Host1, Port1}, _Index} = Id, _Child, _Type, _Modules}) when Tube=:=Tube1,Role=:=Role1,Host=:=Host1,Port=:=Port1-> 
				supervisor:restart_child(?MODULE, Id);
           (_) -> 
				ignore
        end,
    lists:foreach(F, supervisor:which_children(?MODULE)).
