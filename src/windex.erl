-module(windex).

-export([lookup/2, add/2, delete/2, export/1]).
-compile(export_all).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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

delete(Index, Rev) -> ok.

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
convertNode(Value) when is_list(Value) -> {#{}, Value}.

makeTree(Revision) when is_map(Revision) -> {doMakeTree(Revision), []}.
doMakeTree(Revision) when is_map(Revision) ->
        maps:map(fun(_Key, Value) -> convertNode(Value) end, Revision).

mergeTrees({LsubTree, Ldata}, {RsubTree, Rdata}) ->
        NewData = lists:append(Ldata, Rdata),
        {hashMerge(LsubTree, RsubTree), NewData}.

hashMerge(Left, Right) when is_map(Left), is_map(Right) -> hashMerge(Left, maps:to_list(Right));
hashMerge(Left, [])    when is_map(Left) -> Left;
hashMerge(Left, [{Key, Value} |Rest]) when is_map(Left) ->
        NewValue = case maps:find(Key, Left) of
                error -> Value;
                {ok, LeftValue} -> mergeTrees(LeftValue, Value)
        end,
        hashMerge(maps:put(Key, NewValue, Left), Rest).
