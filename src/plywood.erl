-module(plywood).

%% Setup exports
-export([start/0]).

%% Lookup/update exports
-export([lookup/2, lookup/3, add/2, delete/2, deleteByValue/2]).

%% Processing exports
-export([processTree/2, processOps/0]).

-compile(export_all).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() -> application:ensure_all_started(plywood).

lookup(Index, KeyParts) when is_list(KeyParts) ->
	Key = makeNodeID(KeyParts, <<"/">>),
        lookup(Index, Key);
lookup(Index, Key) when is_binary(Key) ->
        lookup(Index, Key, infinity).

lookup(Index, KeyParts, Depth) when is_list(KeyParts) ->
	Key = makeNodeID(KeyParts, <<"/">>),
        lookup(Index, Key, Depth);
lookup(Index, Key, Depth) when is_binary(Key) ->
        doLookup({Index, Key}, 0, Depth).

add(Index, Rev) when is_map(Rev) ->
        makeTree(Index, Rev, fun mergeStore/2).

delete(Index, Rev) when is_map(Rev) ->
        makeTree(Index, Rev, fun deMergeStore/2).

deleteByValue(_Index, _Value) -> ok.

processOps() -> [aggregate, filter].

processTree(Tree, Opts) when is_map(Opts) ->
        OpList = buildOpList(undefined, [], Opts, processOps(), []),
        applyTransforms(Tree, OpList).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

doLookup(NodeKey, Depth, MaxDepth) when Depth < MaxDepth ->
	{ok, Node} = plywood_db:fetch(primary_tree, NodeKey),
	case maps:find(children, Node) of
		{ok, Children} -> maps:put(children, [ doLookup(Child, Depth+1, MaxDepth) || Child <- Children], Node);
		error -> Node
	end;
doLookup(NodeKey, Depth, MaxDepth) when Depth >= MaxDepth ->
	{ok, Node} = plywood_db:fetch(primary_tree, NodeKey),
	maps:remove(children, Node).


makeTree(Index, Revision, Op) when is_map(Revision) -> doMakeTree(Index, [{[{<<"">>, Revision}], []}], Op).

doMakeTree(_, [], _) -> ok;
doMakeTree(Index, [{[], _Path} | Rest], Op) -> doMakeTree(Index, Rest, Op);
doMakeTree(Index, [{[{Name, SubTree} | RestLevel], Path} | Rest], Op) when is_map(SubTree) ->
        Loc = case size(Name) of
		0 -> Path;
		_ -> [Name |Path]
	end,
        Children = maps:to_list(SubTree),
        NewLevel = {Children, Loc},
        Node = #{
		id   => makeNodeID(lists:reverse(Loc), <<"/">>),
		name => Name,
		children => lists:usort([
                        {Index, makeNodeID(lists:reverse([ChildName |Loc]), <<"/">>)}
                                || {ChildName, _ } <- Children
                ]),
                hasChildren => true,
                hasData => false
        },
        ok = Op(Index, Node),
        doMakeTree(Index, [NewLevel, {RestLevel, Path} | Rest], Op);
doMakeTree(Index, [{[{Name, Data} | RestLevel], Path} | Rest], Op) when is_list(Data) ->
        Loc = case size(Name) of
		0 -> Path;
		_ -> [Name |Path]
	end,
        Node = #{
		id   => makeNodeID(lists:reverse(Loc), <<"/">>),
		name => Name,
		data => lists:usort(Data),
                hasChildren => false,
                hasData => true
        },
        ok = Op(Index, Node),
        doMakeTree(Index, [{RestLevel, Path} | Rest], Op).

mergeStore(Index, #{id := Id} = Data) ->
        Key = {Index, Id},
        NewNode = case plywood_db:getIfExists(primary_tree, Key) of
                false -> Data;
                {ok, OldNode} -> mergeNodes(OldNode, maps:to_list(Data))
        end,
        plywood_db:store(primary_tree, Key, NewNode).

deMergeStore(Index, #{id := Id} = Data) ->
        Key = {Index, Id},
        case plywood_db:getIfExists(primary_tree, Key) of
                false -> ok;
                {ok, OldNode} ->
			case deMergeNodes(OldNode, Data) of
				noop -> ok;
				NewNode -> plywood_db:store(primary_tree, Key, NewNode)
			end
        end.

mergeNodes(Left, []) -> Left;
mergeNodes(Left, [{id,   _} |Right]) -> mergeNodes(Left, Right);
mergeNodes(Left, [{name, _} |Right]) -> mergeNodes(Left, Right);
mergeNodes(#{data := Old} =Left, [{data, New} |Right]) ->
        mergeNodes(maps:put(data, lists:umerge(Old, New), Left), Right);
mergeNodes(#{children := Old} =Left, [{children, New} |Right]) ->
        mergeNodes(maps:put(children, lists:umerge(Old, New), Left), Right);
mergeNodes(#{hasChildren := Old} =Left, [{hasChildren, New} |Right]) ->
        mergeNodes(maps:put(hasChildren, (Old or New), Left), Right);
mergeNodes(#{hasData := Old} =Left, [{hasData, New} |Right]) ->
        mergeNodes(maps:put(hasData, (Old or New), Left), Right).

compactNode(Node) when is_map(Node) ->
        maps:from_list([ {K, V} || {K, V} <- maps:to_list(Node), V =/= {#{}, []}]).

deMergeNodes(#{ data := OldData } = Old, #{ data := PurgeData }) ->
	case remove(OldData, PurgeData) of
		OldData -> noop;
		NewData -> maps:put(data, NewData, Old)
	end;
deMergeNodes(_, _) -> noop.

remove(From, Items) when is_list(From), is_list(Items) ->
        ordsets:to_list(ordsets:subtract(ordsets:from_list(From), ordsets:from_list(Items))).

makeNodeID([], _Sep) -> <<"/">>;
makeNodeID([<<"/">>], _Sep) -> <<"/">>;
makeNodeID(Parts, Sep) -> << << Sep/binary, Part/binary>> || Part <- Parts, Part =/= <<"/">> >>.

traverse(Operator, {SubTree, Data}, Path) when is_function(Operator), is_map(SubTree), is_list(Data), is_list(Path) ->
	[ Operator(Datum, Path) || Datum <- Data],
	[ ok = traverse(Operator, Value, [Name | Path]) || {Name, Value} <- maps:to_list(SubTree)],
	ok.

accumulate(Operator, Acc, {SubTree, Data}, Path) when is_function(Operator), is_map(SubTree), is_list(Data), is_list(Path) ->
	AccLevel = lists:foldl(fun(Datum, AccIn) -> Operator(Datum, AccIn, Path) end, Acc, Data),
	lists:foldl(fun({Name, Value}, AccSub)-> accumulate(Operator, AccSub, Value, [Name | Path]) end, AccLevel, maps:to_list(SubTree)).

filter(Operator, {SubTree, Data}, Path) when is_function(Operator), is_map(SubTree), is_list(Data), is_list(Path) ->
	NewData = [ Datum || Datum <- Data, Operator(Datum, Path)],
	{compactNode(maps:map(fun(Name, Value) -> filter(Operator, Value, [Name | Path]) end, SubTree)), NewData}.

map(Operator, {SubTree, Data}, Path) when is_function(Operator), is_map(SubTree), is_list(Data), is_list(Path) ->
	NewData = [ Operator(Datum, Path) || Datum <- Data],
	{maps:map(fun(Name, Value) -> map(Operator, Value, [Name | Path]) end, SubTree), NewData}.

transform(Operator, {SubTree, Data} = Node, Path) when is_function(Operator), is_map(SubTree), is_list(Data), is_list(Path) ->
        case Operator(Node, Path) of
                {continue, {NewSubTree, NewData}} ->
                        {maps:map(fun(Name, Value) -> transform(Operator, Value, [Name | Path]) end, NewSubTree), NewData};
                {done, NewNode} -> NewNode
        end.

getOperator(aggregate, {Field, max}) -> fun(Tree) -> Tree end;
getOperator(aggregate, {Field, min}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '>', Value}) ->
        buildFilterFun(fun(A) -> A > Value end, Field);
getOperator(filter, {Field, '<', Value}) ->
        buildFilterFun(fun(A) -> A < Value end, Field);
getOperator(filter, {Field, '>=', Value}) ->
        buildFilterFun(fun(A) -> A >= Value end, Field);
getOperator(filter, {Field, '=<', Value}) ->
        buildFilterFun(fun(A) -> A =< Value end, Field);
getOperator(filter, {Field, '=', Value}) ->
        buildFilterFun(fun(A) -> A == Value end, Field);
getOperator(filter, {Field, '!=', Value}) ->
        buildFilterFun(fun(A) -> A /= Value end, Field);
getOperator(filter, {Field, 'like', Value}) ->
        Regexp = buildRegexFun(Value, false, []),
        buildFilterFun(Regexp, Field);
getOperator(filter, {Field, 'ilike', Value}) ->
        Regexp = buildRegexFun(Value, false, [caseless]),
        buildFilterFun(Regexp, Field);
getOperator(filter, {Field, 'unlike', Value}) ->
        Regexp = buildRegexFun(Value, true, []),
        buildFilterFun(Regexp, Field);
getOperator(filter, {Field, 'unilike', Value}) ->
        Regexp = buildRegexFun(Value, true, [caseless]),
        buildFilterFun(Regexp, Field);
getOperator(_, _) -> fun(Tree) -> Tree end.

buildOpList(Token, [Op | Rest], OpsMap, Seq, List) ->
        Func = getOperator(Token, Op),
        buildOpList(Token, Rest, OpsMap, Seq, [Func | List]);
buildOpList(_Previous, [], OpsMap, [Class | Rest], List) ->
        case maps:find(Class, OpsMap) of
                error -> buildOpList(Class, [], maps:remove(Class, OpsMap), Rest, List);
                {ok, Items} when is_list(Items) -> buildOpList(Class, Items, maps:remove(Class, OpsMap), Rest, List);
                {ok, Item}  -> buildOpList(Class, [Item], maps:remove(Class, OpsMap), Rest, List)
        end;
buildOpList(_Last, [], #{}, _Seq, List) -> List.

applyTransforms(Tree, []) -> Tree;
applyTransforms(Tree, [Op | Rest]) ->
        NewTree = Op(Tree),
        applyTransforms(NewTree, Rest).

buildFilterFun(Op, <<"name">>) -> 
        fun(Tree) ->
                filter(fun(_Data, [Name |_Path]) -> Op(Name) end, Tree, [])
        end;
buildFilterFun(Op, <<"path">>) -> 
        fun(Tree) ->
                filter(fun(_Data, Path) ->
                        Op(makeNodeID(lists:reverse(Path), <<"/">>))
                end, Tree, [])
        end;
buildFilterFun(Op, Field) ->
        fun(Tree) ->
                filter(fun(Data, _Path) when is_map(Data) ->
                        case maps:find(Field, Data) of
                                {ok, DataValue} -> Op(DataValue);
                                _ -> false
                        end;
                        (_Data, _Path) -> false
                end, Tree, [])
        end.

buildRegexFun(Pattern, Invert, Opts) ->
        {ok, Regexp} = re:compile(Pattern, Opts),
        fun(DataValue) ->
                (match == re:run(DataValue, Regexp, [{capture, none}])) xor Invert
        end.
