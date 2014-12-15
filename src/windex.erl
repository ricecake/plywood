-module(windex).

-export([start/0]).
-export([lookup/2, add/2, delete/2, deleteByValue/2, export/1]).

-compile(export_all).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() -> application:ensure_all_started(windex).

lookup(Index, Key) ->
        KeyParts = binary:split(Key, <<"/">>, [global]),
        {ok, RootNode} = windex_db:fetch(primary_tree, Index),
        doLookup(KeyParts, RootNode).

add(Index, Rev) when is_map(Rev) ->
        RevTree = makeTree(Rev),
        NewTree = case windex_db:getIfExists(primary_tree, Index) of
                false -> RevTree;
                {ok, FullTree} -> mergeTrees(FullTree, RevTree)
        end,
        windex_db:store(primary_tree, Index, NewTree).

delete(Index, Rev) when is_map(Rev) ->
        RevTree = makeTree(Rev),
        {ok, FullTree} = windex_db:fetch(primary_tree, Index),
        windex_db:store(primary_tree, Index, demergeTrees(FullTree, RevTree)).

deleteByValue(Index, Value) -> ok.

export({SubTree, Data}) when is_map(SubTree)->
        #{
                data    => Data,
                subtree => maps:map(fun(_Key, Value) -> export(Value) end, SubTree)
        };
export(Index) ->
        {ok, RootNode} = windex_db:fetch(primary_tree, Index),
        export(RootNode).
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
