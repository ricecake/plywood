-module(plywood).

%% Setup exports
-export([start/0]).

%% Lookup/update exports
-export([lookup/2, lookup/3, add/2, delete/2, deleteByValue/3]).

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

compact(Index) when is_binary(Index) ->
        doCompaction([{{Index, <<"/">>}, undefined}]).

deleteByValue(Index, Field, Value) when is_binary(Index), is_binary(Field) ->
	ok.
	%traverse(fun(Datum, _, _) when is_map(Datum) ->
	%
	%)

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
        case plywood_db:getIfExists(primary_tree, Key) of
                false -> plywood_db:asyncStore(primary_tree, Key, Data);
                {ok, OldNode} ->
			case mergeNodes(OldNode, maps:to_list(Data)) of
				OldNode -> ok;
				NewNode -> plywood_db:asyncStore(primary_tree, Key, NewNode)
			end
        end.

deMergeStore(Index, #{id := Id} = Data) ->
        Key = {Index, Id},
        case plywood_db:getIfExists(primary_tree, Key) of
                false -> ok;
                {ok, OldNode} ->
			case deMergeNodes(OldNode, Data) of
				noop -> ok;
				NewNode -> plywood_db:asyncStore(primary_tree, Key, NewNode)
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

traverse(_Operator, []) -> ok;
traverse(Operator, [NodeKey |Rest]) when is_function(Operator), is_tuple(NodeKey) ->
	{ok, #{ id := Id, name := Name } = Node} = plywood_db:fetch(primary_tree, NodeKey),
	case maps:find(data, Node) of
		{ok, Data} -> [ Operator(Datum, Id, Name) || Datum <- Data];
		error -> ok
	end,
	traverse(Operator, fastConcat(Rest, maps:get(children, Node, []))).

accumulate(_Operator, Acc, []) -> Acc;
accumulate(Operator, Acc, [NodeKey |Rest]) when is_function(Operator), is_tuple(NodeKey) ->
	{ok, #{ id := Id, name := Name } = Node} = plywood_db:fetch(primary_tree, NodeKey),
	NodeAcc = case maps:find(data, Node) of
		{ok, Data} -> lists:foldl(fun(Datum, AccIn) -> Operator(Datum, AccIn, Id, Name) end, Acc, Data);
		error -> Acc
	end,
	accumulate(Operator, NodeAcc, fastConcat(Rest, maps:get(children, Node, []))).


recurseOnChildren(Func, Operator, #{ children := Children } = Node) ->
	maps:put(children, [Func(Operator, Child) || Child <- Children], Node);
recurseOnChildren(_Func, _Op, Node) -> Node.

filter(Operator, NodeKey) when is_function(Operator), is_tuple(NodeKey) ->
	{ok, #{ id := Id, name := Name } = Node} = plywood_db:fetch(primary_tree, NodeKey),
	NewNode = case maps:find(data, Node) of
		{ok, Data} -> maps:put(data, [ Datum || Datum <- Data, Operator(Datum, Id, Name)], Node);
		error -> Node
	end,
	case maps:find(children, NewNode) of
		{ok, Children} -> maps:put(children, [filter(Operator, Child) || Child <- Children], NewNode);
		error -> NewNode
	end.

map(Operator, NodeKey) when is_function(Operator), is_tuple(NodeKey) ->
	{ok, #{ id := Id, name := Name } = Node} = plywood_db:fetch(primary_tree, NodeKey),
	NewNode = case maps:find(data, Node) of
		{ok, Data} -> maps:put(data, [ Operator(Datum, Id, Name) || Datum <- Data], Node);
		error -> Node
	end,
	case maps:find(children, NewNode) of
		{ok, Children} -> maps:put(children, [map(Operator, Child) || Child <- Children], NewNode);
		error -> NewNode
	end.

transform(Operator, NodeKey) when is_function(Operator), is_tuple(NodeKey) ->
	{ok, #{ id := Id, name := Name } = Node} = plywood_db:fetch(primary_tree, NodeKey),
	NewNode = case maps:find(data, Node) of
		{ok, Data} ->
			NewData = case Operator(Data, Id, Name) of
				TransformedData when is_list(TransformedData) -> TransformedData;
				TransformedDatum -> [TransformedDatum]
			end,
			maps:put(data, NewData, Node);
		error -> Node
	end,
	case maps:find(children, NewNode) of
		{ok, Children} -> maps:put(children, [transform(Operator, Child) || Child <- Children], NewNode);
		error -> NewNode
	end.

rewrite(Operator, NodeKey) when is_function(Operator), is_tuple(NodeKey) ->
	{ok, Node} = plywood_db:fetch(primary_tree, NodeKey),
	NewNode = Operator(Node),
	NewChildren = maps:get(children, NewNode, []),
	OldChildren = maps:get(children,    Node, []),
	PurgeChildren = remove(OldChildren, NewChildren),
	erase(PurgeChildren),
	rewrite(Operator, fastConcat(Rest, Children));

erase([]) -> ok;
erase([NodeKey |Rest]) when is_tuple(NodeKey) ->
	{ok, #{ id := Id, name := Name } = Node} = plywood_db:fetch(primary_tree, NodeKey),
	plywood_db:delete(primary_tree, NodeKey),
	erase(fastConcat(Rest, maps:get(children, Node, []))).

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
                filter(fun(_Data, _Id, Name) -> Op(Name) end, Tree)
        end;
buildFilterFun(Op, <<"path">>) -> 
        fun(Tree) ->
                filter(fun(_Data, Id, _Name) -> Op(Id) end, Tree)
        end;
buildFilterFun(Op, Field) ->
        fun(Tree) ->
                filter(fun(Data, _Id, _Name) when is_map(Data) ->
                        case maps:find(Field, Data) of
                                {ok, DataValue} -> Op(DataValue);
                                _ -> false
                        end;
                        (_Data, _Id, _Name) -> false
                end, Tree)
        end.

buildRegexFun(Pattern, Invert, Opts) ->
        {ok, Regexp} = re:compile(Pattern, Opts),
        fun(DataValue) ->
                (match == re:run(DataValue, Regexp, [{capture, none}])) xor Invert
        end.

fastConcat([], B) -> B;
fastConcat(A,B) when length(A) > length(B) -> fastConcat(B,A);
fastConcat(A,B) -> A++B.

purge(NodeKey, undefined) -> plywood_db:delete(primary_tree, NodeKey);
purge({Index, _ChildId} = NodeKey, #{ children := Children, id := Id } = Parent) ->
        NewParent = maps:put(children, [ Child || Child <- Children, Child /= NodeKey], Parent),
        ok = plywood_db:store(primary_tree, {Index, Id}, NewParent),
        purge(NodeKey, undefined).

doCompaction([]) -> ok;
doCompaction([{CurrNode, Parent} |Rest]) ->
        case checkPrune(CurrNode) of
                {true, []} ->
                        ok = purge(CurrNode, Parent),
                        doCompaction(Rest);
                {false, Candidates} -> doCompaction(fastConcat(Rest, Candidates))
        end.

checkPrune(Id) ->
        {ok, Node} = plywood_db:fetch(primary_tree, Id),
        Data = case maps:find(data, Node) of
                error       -> false;
                {ok, []}    -> false;
                {ok, _Data} -> true
        end,
        {Child, Candidates} = case maps:find(children, Node) of
                {ok, Children} -> checkChildren(Node, Children);
                error -> {false, []}
        end,
        Prune = not (Child or Data),
        {Prune, Candidates}.

checkChildren(_, []) -> {false, []};
checkChildren(Parent, [Child |Rest]) ->
        case checkPrune(Child) of
                {true, []} ->
                        purge(Child, Parent),
                        checkChildren(Parent, Rest);
                {false, Candidates} ->
                        {true, fastConcat(Candidates, [{Candidate, Parent} || Candidate <- Rest])}
        end.
