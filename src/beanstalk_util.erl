-module(beanstalk_util).

-author('thijsterlouw@gmail.com').

-export([now/0, get_app_env/2, reload_config/1, sup_child_spec/3, sup_child_spec/2]).

now() ->
	{MegaS, S, _MicroS} = erlang:now(),
	MegaS*1000000 + S.

get_app_env(Opt, Default) ->
	case application:get_env(Opt) of
		{ok, Val} -> Val;
		_ ->
			case init:get_argument(Opt) of
				{ok, [[Val | _]]} 	-> Val;
				error       		-> Default
			end
	end.

format_atom(Format, Args) ->
    list_to_atom(lists:flatten(io_lib:format(Format, Args))).
    % }}}

% 是否在本node中启动该进程, 如果不设置，缺省为启动
module_enabled(Module) ->
    module_enabled(Module, 1).

module_enabled(Module, DefaultCount) ->
    Key = format_atom("enable_~p", [Module]),
    get_app_env(Key, DefaultCount).

sup_child_spec(Module, Fun, Count) ->
    N = module_enabled(Module, Count),
    lists:map(fun(I) ->
        % Id = format_atom("~p_~p", [Module, I]),
        Fun(Module, {Module, I})
    end, lists:seq(0, N-1)).

sup_child_spec(Module, Fun) ->
    sup_child_spec(Module, Fun, 1).


%supply a list of application names to restart (as atoms). 
%function will only reload config for apps already started
reload_config(AppNames) ->
	case init:get_argument(config) of
    		{ok, [ Files ]} ->
	         	ConfFiles = [begin
		                          S = filename:basename(F,".config"),
		                          filename:join(filename:dirname(F),  S ++ ".config")
		                      end || F <- Files],
	                      
	         	% Move sys.config to the head of the list
	         	Config = lists:sort(fun("sys.config", _) -> true;
	                                		(_, _) -> false 
	                                	end, 
	                                	ConfFiles),

	         	OldEnv = application_controller:prep_config_change(),

			WhichApplicationNamesSet =  sets:from_list([ A || {A,_,_} <- application:which_applications()]),
			AppNamesSet = sets:from_list(AppNames),
			AllowedApps = sets:intersection(WhichApplicationNamesSet, AppNamesSet),
			
	         	Apps = [{application, A, make_appl(A)} || A <- sets:to_list(AllowedApps)],
	         	application_controller:change_application_data(Apps, Config),
	         	application_controller:config_change(OldEnv);
		_ ->
	       	{ok, []}
     end.

make_appl(App) when is_atom(App) ->
	AppList  = element(2, application:get_all_key(App)),
	FullName = code:where_is_file(atom_to_list(App) ++ ".app"),
	case file:consult(FullName) of
		{ok, [{application, _, Opts}]} ->
			 Env = proplists:get_value(env, Opts, []),
			 lists:keyreplace(env, 1, AppList, {env, Env});
		{error, _Reason} ->
	 		lists:keyreplace(env, 1, AppList, {env, []})
	end.


