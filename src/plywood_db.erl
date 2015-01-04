-module(plywood_db).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_link/2]).
-export([store/3, fetch/2, exists/2, getIfExists/2, delete/2]).
-export([asyncStore/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name) -> start_link(Name, "./").

start_link(Name, Base) ->
	File = lists:append([Base, atom_to_list(Name), ".lvldb"]),
	gen_server:start_link({local, Name}, ?MODULE, #{ file => File }, []).

asyncStore(Db, Key, Value) ->
	spawn(fun()->
		store(Db, Key, Value)
	end),
	ok.

store(Db, Key, Value) -> eleveldb:put(getRef(Db), term_to_binary(Key), term_to_binary(Value), []).
fetch(Db, Key)        ->
        case eleveldb:get(getRef(Db), term_to_binary(Key), []) of
                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
                not_found        -> throw({not_found, Key})
        end.

exists(Db, Key) ->
        case eleveldb:get(getRef(Db), term_to_binary(Key), []) of
                {ok, _BinaryTerm}  -> true;
                not_found         -> false
        end.

getIfExists(Db, Key) ->
        case eleveldb:get(getRef(Db), term_to_binary(Key), []) of
                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
                not_found         -> false
        end.

delete(Db, Key) ->
        eleveldb:delete(getRef(Db), term_to_binary(Key), []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#{ file := File } = Args) ->
	{ok, Ref} = eleveldb:open(File, [{create_if_missing, true}, {compression, true}, {use_bloomfilter, true}]),
	{ok, Args#{ ref => Ref }}.

handle_call(get_ref, _From, #{ref := Ref} = State) ->
	{reply, Ref, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

getRef(ID) when is_atom(ID) ->
	case get({db_ref, ID}) of
		undefined ->
			Ref = gen_server:call(ID, get_ref, infinity),
			put({db_ref, ID}, Ref),
			Ref;
		Ref       -> Ref
	end.
