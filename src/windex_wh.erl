-module(windex_wh).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([lookup/2, insert/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"json">>, []}, lookup}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
                {{<<"text">>, <<"json">>, []}, insert}
        ], Req, State}.

resource_exists(Req, _State) ->
	case cowboy_req:binding(index, Req) of
		undefined -> {true, Req, index};
		Index ->
			case windex_db:exists(primary_tree, Index) of
				true  -> {true, Req, Index};
				false -> {false, Req, Index}
			end
	end.

insert(Req, State) ->
	Index = cowboy_req:binding(index, Req),
	{ok, Data, Req2} = cowboy_req:body(Req),
	ok = windex:add(Index, jiffy:decode(Data, [return_maps])),
	case cowboy_req:method(Req2) of
		<<"POST">> ->
			{{true, <<"/t/", Index/binary>>}, Req2, State};
		_ ->
			{true, Req2, State}
	end.

lookup(Req, Index) ->
        io:format("~p~n", [{Req, Index, cowboy_req:path_info(Req)}]),
        Path = cowboy_req:path_info(Req),
        try windex:lookup(Index, Path) of
                Data ->
                        Result = windex:export(Data),
                        { jiffy:encode(Result), Req, Index }
        catch
                Exception:Reason -> {"Null", Req, Index}
        end.
