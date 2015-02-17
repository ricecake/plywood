-module(plywood_wh).

-export([init/2]).

init(Req, Opts) ->
	Method  = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2    = process(Method, HasBody, Req),
	{ok, Req2, Opts}.

process(<<"GET">>, _Body, Req) ->
	Index = cowboy_req:binding(index, Req),
	Opts  = maps:from_list(cowboy_req:parse_qs(Req)),
	Path  = cowboy_req:path_info(Req),
	lookup(Index, Path, Opts, Req);

process(<<"POST">>, _Body, Req) ->
	Index            = cowboy_req:binding(index, Req),
	{ok, Opts, Req2} = cowboy_req:body(Req, [{length, 1000000}]),
	Path             = cowboy_req:path_info(Req2),
	lookup(Index, Path, jiffy:decode(Opts, [return_maps]), Req2);

process(<<"PUT">>, true, Req) ->
	Index            = cowboy_req:binding(index, Req),
	{ok, Data, Req2} = cowboy_req:body(Req, [{length, 100000000}]),
	insert(Index, Data, Req2);

process(<<"PUT">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

process(<<"DELETE">>, true, Req) ->
	Index = cowboy_req:binding(index, Req),
	{ok, Data, Req2} = cowboy_req:body(Req, [{length, 100000000}]),
	remove(Index, Data, Req2);

process(<<"DELETE">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

process(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

lookup(Index, Path, Opts, Req) ->
	NewOpts = plywood_wh_utils:cleanOptions(maps:to_list(Opts), #{}),
	case plywood_worker:lookup(Index, Path, NewOpts) of
		{ok, JSON}         ->
			cowboy_req:reply(200, [
				{<<"content-type">>, <<"text/json; charset=utf-8">>}
			], JSON, Req);
		{not_found, Index} -> cowboy_req:reply(404, [], <<"Not Found">>, Req);
		_Error             -> cowboy_req:reply(418, [], <<"Error">>, Req)
	end.

insert(Index, Data, Req) ->
	ok = plywood_worker:add(Index, Data),
	cowboy_req:reply(200, Req).

remove(Index, Data, Req) ->
	ok = plywood_worker:delete(Index, Data),
	cowboy_req:reply(200, Req).

