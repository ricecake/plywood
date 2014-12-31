-module(plywood_db).

-export([start_link/1, start_link/2]).
-export([open/1, open/2, close/1, store/3, fetch/2, exists/2, getIfExists/2]).

start_link(Name) -> open(Name).
start_link(Name, Base) -> open(Name, Base).

open(Name) when is_atom(Name) -> open(Name, "./").
open(Name, Base) when is_atom(Name), is_list(Base) ->
        File = lists:append([Base, atom_to_list(Name), ".hanoidb"]),
        hanoidb:open_link({local, Name}, File, [{compress, snappy}, {top_level, 15}, {page_size, 16384}, {sync_strategy, none}]).

close(Db) -> hanoidb:close(Db).

store(Db, Key, Value) -> hanoidb:put(Db, term_to_binary(Key), term_to_binary(Value)).
fetch(Db, Key)        ->
        case hanoidb:get(Db, term_to_binary(Key)) of
                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
                not_found        -> throw({not_found, Key})
        end.

exists(Db, Key) ->
        case hanoidb:get(Db, term_to_binary(Key)) of
                {ok, _BinaryTerm}  -> true;
                not_found         -> false
        end.

getIfExists(Db, Key) ->
        case hanoidb:get(Db, term_to_binary(Key)) of
                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
                not_found         -> false
        end.
