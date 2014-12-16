-module(windex_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = windex_sup:start_link(),
    Dispatch = cowboy_router:compile([
	    {'_', [
			{"/tree/:index/[...]", windex_wh, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 25, [{ip, {127,0,0,1}}, {port, 8080}],
        				[{env, [{dispatch, Dispatch}]}]),
	{ok, Pid}.

stop(_State) ->
    ok.
