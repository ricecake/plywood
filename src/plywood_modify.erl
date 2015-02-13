-module(plywood_modify).

-export([init/2]).

init(Req, Opts) ->
	Method  = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2    = process(Method, HasBody, Req),
	{ok, Req2, Opts}.

process(<<"POST">>, _Body, Req) ->
	% TODO
	ok;

process(_, _, Req) ->
	%% Bad Request
	cowboy_req:reply(400, Req).

