-module(plywood).

%% Setup exports
-export([start/0]).

%% Lookup/update exports
-export([lookup/2, add/2, delete/2, deleteByValue/2]).

%% Processing exports
-export([export/1, export/2, truncate/2]).
-export([processTree/2]).

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
        RevTree = makeTree(Rev),
        NewTree = case plywood_db:getIfExists(primary_tree, Index) of
                false -> RevTree;
                {ok, FullTree} -> mergeTrees(FullTree, RevTree)
        end,
        plywood_db:store(primary_tree, Index, NewTree).

delete(Index, Rev) when is_map(Rev) ->
        RevTree = makeTree(Rev),
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

processTree(Tree, Opts)-> ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

doLookup([<<>> | Rest], Node) -> doLookup(Rest, Node);
doLookup([], Node) -> Node;
doLookup([Label | Rest], {Node, _Data}) when is_map(Node) ->
        NewNode = maps:get(Label, Node),
        doLookup(Rest, NewNode).

convertNode(Value) when is_map(Value) -> {doMakeTree(Value), []};
convertNode(Value) when is_list(Value) -> {#{}, lists:usort(Value)}.

makeTree(Revision) when is_map(Revision) -> {doMakeTree(Revision), []}.
doMakeTree(Revision) when is_map(Revision) ->
        compactNode(maps:map(fun(_Key, Value) -> convertNode(Value) end, Revision)).

mergeTrees({LsubTree, Ldata}, {RsubTree, Rdata}) ->
        NewData = lists:umerge(Ldata, Rdata),
        {hashMerge(LsubTree, RsubTree), NewData}.

hashMerge(Left, Right) when is_map(Left), is_map(Right) -> hashMerge(Left, maps:to_list(Right));
hashMerge(Left, [])    when is_map(Left) -> Left;
hashMerge(Left, [{Key, Value} |Rest]) when is_map(Left) ->
        NewValue = case maps:find(Key, Left) of
                error -> Value;
                {ok, LeftValue} -> mergeTrees(LeftValue, Value)
        end,
        hashMerge(maps:put(Key, NewValue, Left), Rest).

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

proccesOpPriority() -> [aggregate, filter, truncate].

getOperator(aggregate, {Field, max}) -> fun(Tree) -> Tree end;
getOperator(aggregate, {Field, min}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '>', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '<', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '>=', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '<=', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '=', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '!=', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '=~', Value}) -> fun(Tree) -> Tree end;
getOperator(filter, {Field, '!~', Value}) -> fun(Tree) -> Tree end;
getOperator(truncate, Depth) -> fun(Tree) -> Tree end;
        fun(Tree) -> truncate(Tree, Depth) end.

buildOpList(Ops) when is_map(Ops) -> ok.

applyTransforms(Tree, []) -> Tree;
applyTransforms(Tree, [Op | Rest]) ->
        NewTree = Op(Tree),
        applyTransforms(NewTree, Rest).