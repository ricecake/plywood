-module(windex).

-export([lookup/2, add/2, delete/2]).
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
        case windex_db:getIfExists(primary_tree, Index) of
                false -> windex_db:store(primary_tree, Index, RevTree)
        end.

delete(Index, Rev) -> ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

doLookup([<<>> | Rest], Node) -> doLookup(Rest, Node);
doLookup([], Node) -> Node;
doLookup([Label | Rest], {Node, _Data}) when is_map(Node) ->
        NewNode = maps:get(Label, Node),
        doLookup(Rest, NewNode).

convertNode(Value) when is_map(Value) -> {doMakeTree(Value), undefined};
convertNode(Value) when is_list(Value) -> {undefined, Value}.

makeTree(Revision) when is_map(Revision) -> {doMakeTree(Revision), undefined}.
doMakeTree(Revision) when is_map(Revision) ->
        maps:map(fun(_Key, Value) -> convertNode(Value) end, Revision).
