-module(windex_db).

-export([open/1, open/2, close/1, store/3, fetch/2, getIfExists/2]).

open(Name) when is_atom(Name) -> open(Name, "./").
open(Name, Base) when is_atom(Name), is_list(Base) ->
        File = lists:append([Base, atom_to_list(Name), ".hanoidb"]),
        hanoidb:open_link({local, Name}, File, [{compress, snappy}]).

close(Db) -> hanoidb:close(Db).

store(Db, Key, Value) -> hanoidb:put(Db, term_to_binary(Key), term_to_binary(Value)).
fetch(Db, Key)        ->
        case hanoidb:get(Db, term_to_binary(Key)) of
                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
                not_found        -> throw({not_found, Key})
        end.

getIfExists(Db, Key) ->
        case hanoidb:get(Db, term_to_binary(Key)) of
                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
                not_found         -> false
        end.
