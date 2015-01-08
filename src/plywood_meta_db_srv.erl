-module(plywood_meta_db_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, #{}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	{ok, Args}.

handle_call({reclaim, Table}, From, State) ->
	{ok, Tid} = TableInfo = maps:find(Table, State),
	ets:give_away(Tid, From, Table),
	{reply, TableInfo, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'ETS-TRANSFER', Tid, FromPid, HeirData}, State) ->
	{noreply, maps:put(HeirData, Tid, State)};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
