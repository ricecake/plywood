-module(plywood_modify).

-export([init/2]).

init(Req, Opts) ->
	Method  = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2    = process(Method, HasBody, Req),
	{ok, Req2, Opts}.

process(<<"POST">>, _Body, Req) ->
	Index             = cowboy_req:binding(index, Req),
	{ok, Opts, Req2}  = cowboy_req:body(Req, [{length, 1000000}]),
	mutate(Index, jiffy:decode(Opts, [return_maps]), Req2);

process(_, _, Req) ->
	%% Bad Request
	cowboy_req:reply(400, Req).

mutate(Index, Spec, Req) ->
	NewSpec = plywood_wh_utils:cleanOptions(maps:to_list(Spec), #{}),
	case plywood_worker:mutate(Index, NewSpec) of
		ok ->
			cowboy_req:reply(200,
				[{<<"content-type">>, <<"text/json; charset=utf-8">>}],
				Index,
				Req
			);
		{not_found, Index} -> cowboy_req:reply(404, [], <<"Not Found">>, Req);
		_Error             -> cowboy_req:reply(500, [], <<"Error">>, Req)
	end.

