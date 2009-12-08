-module(beanstalk_app).
-author('echou327@gmail.com').

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, config_change/3]).
-export([init/1]).


-export([start/0, start_link/0, stop/0]).

% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    application:start(beanstalk).

stop() ->
    application:stop(beanstalk).

% application callbacks

start(_Type, _Args) ->
    ?MODULE:start_link().

stop(_State) ->
    ok.

config_change(_Changed, _New, _Removed) ->
    ok.

% supervisor callback
init([]) ->
    io:format("[Beanstalk] Starting~n"),
    Specs = specs([{beanstalk_client_sup, 1}, % must be 1
                   {beanstalk_config, 1}]),  % must be 1
    {ok, {{one_for_one, 10, 10}, Specs}}.

% supervisor local functions
specs(Specs) ->
    lists:foldl(fun({Module, Count}, Acc) ->
                    Acc ++ beanstalk_util:sup_child_spec(Module, fun one_spec/2, Count)
                end, [], Specs).

one_spec(beanstalk_client_sup, Id) ->
    {Id, {beanstalk_client_sup, start_link, []}, permanent, infinity, supervisor, []};
one_spec(Module, Id) ->
    {Id, {Module, start_link, []}, permanent, 2000, worker, []}.
