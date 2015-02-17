-module(plywood_wh_utils).

-export([appendMap/3, cleanOptions/2, cleanField/2]).

cleanOptions([], Accum) ->
	Accum;

cleanOptions([{Class, Value} | Rest], Accum) when is_list(Value) ->
	NewValues = [ cleanField(Class, SubValue) || SubValue <- Value],
	NewAcc    = lists:foldl(fun({Key, Val}, Acc) -> appendMap(Key, Val, Acc) end, Accum, NewValues),
	cleanOptions(Rest, NewAcc);

cleanOptions([{Class, Value} | Rest], Accum) ->
	{NewKey, NewVal} = cleanField(Class, Value),
	cleanOptions(Rest, appendMap(NewKey, NewVal, Accum)).

cleanField(<<"depth">>, Depth) when is_binary(Depth) ->
	{depth, erlang:binary_to_integer(Depth)};

cleanField(<<"filter">>, #{ <<"field">> := Field, <<"op">> := Op, <<"value">> := Val }) ->
	{filter, {Field, binary_to_existing_atom(Op, utf8), Val}};

cleanField(Field, Value) ->
	{Field, Value}.

appendMap(Key, Value, Map) ->
	case maps:find(Key, Map) of
		{ok, MapValue} when is_list(MapValue) ->
			maps:put(Key, [Value | MapValue], Map);
		{ok, MapValue} ->
			maps:put(Key, [Value, MapValue], Map);
		error ->
			maps:put(Key, Value, Map)
	end.

