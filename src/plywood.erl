-module(plywood).

%% Setup exports
-export([start/0]).

%% Lookup/update exports
-export([lookup/2, add/2, delete/2, deleteByValue/2]).

%% Processing exports
-export([export/1, export/2, truncate/2]).
-export([processTree/2, processOps/0]).

-compile(export_all).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() -> application:ensure_all_started(plywood).

lookup(Index, Key) when is_binary(Key) ->
        KeyParts = binary:split(Key, <<"/">>, [global]),
        lookup(Index, KeyParts);
lookup(Index, KeyParts) when is_list(KeyParts) ->
        {ok, RootNode} = plywood_db:fetch(primary_tree, Index),
        doLookup(KeyParts, RootNode).

add(Index, Rev) when is_map(Rev) ->
        makeTree(Index, Rev, fun mergeStore/2).

delete(Index, Rev) when is_map(Rev) ->
        RevTree = makeTree(Index, Rev, fun demergeTrees/2),
        {ok, FullTree} = plywood_db:fetch(primary_tree, Index),
        plywood_db:store(primary_tree, Index, demergeTrees(FullTree, RevTree)).

deleteByValue(_Index, _Value) -> ok.

truncate({SubTree, _Data} = Node, Depth) when is_map(SubTree)->
        Truncator = fun(D,P) when length(P) < Depth -> {continue, D}; ({_,D},_)-> {done, {#{}, D}} end,
        transform(Truncator, Node, []).

export({SubTree, _Data} = Node) when is_map(SubTree)-> export([], Node);
export(Index) ->
        {ok, RootNode} = plywood_db:fetch(primary_tree, Index),
        export(RootNode).

export([], {SubTree, _Data} = Node) when is_map(SubTree)-> prepExport(<<"/">>, [], Node);
export([Name |Rest], {SubTree, _Data} = Node) when is_map(SubTree)-> prepExport(Name, Rest, Node);
export(Path, Index) ->
        {ok, RootNode} = plywood_db:fetch(primary_tree, Index),
        export(Path, RootNode).

processOps() -> [aggregate, filter, truncate].

processTree(Tree, Opts) when is_map(Opts) ->
        OpList = buildOpList(undefined, [], Opts, processOps(), []),
        applyTransforms(Tree, OpList).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

doLookup([<<>> | Rest], Node) -> doLookup(Rest, Node);
doLookup([], Node) -> Node;
doLookup([Label | Rest], {Node, _Data}) when is_map(Node) ->
        NewNode = maps:get(Label, Node),
        doLookup(Rest, NewNode).

makeTree(Index, Revision, Op) when is_map(Revision) -> doMakeTree(Index, [{maps:to_list(Revision), []}], Op).

doMakeTree(_, [], _) -> ok;
doMakeTree(Index, [{[], _Path} | Rest], Op) -> doMakeTree(Index, Rest, Op);
doMakeTree(Index, [{[{Name, SubTree} | RestLevel], Path} | Rest], Op) when is_map(SubTree) ->
        Loc = [Name |Path],
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
        Loc = [Name |Path],
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

demergeTrees({LsubTree, Ldata}, {RsubTree, Rdata}) ->
        NewData = remove(Ldata, Rdata),
        NewSubTree = hashDelete(LsubTree, RsubTree),
        {compactNode(NewSubTree), NewData}.

hashDelete(Left, Right) when is_map(Left), is_map(Right) -> hashDelete(Left, maps:to_list(Right));
hashDelete(Left, [])    when is_map(Left) -> Left;
hashDelete(Left, [{Key, Value} |Rest]) when is_map(Left) ->
        NewValue = case maps:find(Key, Left) of
                error -> Value;
                {ok, LeftValue} -> demergeTrees(LeftValue, Value)
        end,
        hashDelete(maps:put(Key, NewValue, Left), Rest).

remove(From, Items) when is_list(From), is_list(Items) ->
        ordsets:to_list(ordsets:subtract(ordsets:from_list(From), ordsets:from_list(Items))).

makeNodeID([], _Sep) -> <<"/">>;
makeNodeID([<<"/">>], _Sep) -> <<"/">>;
makeNodeID(Parts, Sep) -> << << Sep/binary, Part/binary>> || Part <- Parts, Part =/= <<"/">> >>.

prepExport(Name, Path, {SubTree, []})->
	#{
		id   => makeNodeID(lists:reverse([Name |Path]), <<"/">>),
		name => Name,
		children => [ prepExport(Key, [Name |Path], Value) || {Key, Value} <-maps:to_list(SubTree)]
	};
prepExport(Name, Path, {#{}, Data})  ->
	#{
		id   => makeNodeID(lists:reverse([Name |Path]), <<"/">>),
		name => Name,
		data => Data
	};
prepExport(Name, Path, {SubTree, Data}) ->
	#{
		id   => makeNodeID(lists:reverse([Name |Path]), <<"/">>),
		name => Name,
		data => Data,
		children => [ prepExport(Key, [Name |Path], Value) || {Key, Value} <-maps:to_list(SubTree)]
	}.

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
getOperator(truncate, Depth) when is_integer(Depth) ->
        fun(Tree) -> truncate(Tree, Depth) end;
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
