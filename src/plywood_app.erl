-module(plywood_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = plywood_sup:start_link(),
    Dispatch = cowboy_router:compile([
	    {'_', [
			{"/tree/:index/[...]", plywood_wh, []},
			{"/view/:index", plywood_viz, []},
			{"/[...]",     cowboy_static, {priv_dir, plywood, "html/"}},
			{"/js/[...]",  cowboy_static, {priv_dir, plywood, "js/"  }},
			{"/css/[...]", cowboy_static, {priv_dir, plywood, "css/" }}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 25, [{port, 8080}],
        				[
						{compress, true},
						{env, [{dispatch, Dispatch}]}
					]),
	{ok, Pid}.

stop(_State) ->
    ok.
