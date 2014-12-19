-module(plywood_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, add/2, add/3, add/4, delete/2, delete/3, delete/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add(Index, Data) ->
        {ok, Pid} = plywood_work_sup:getWorker(),
        add(Pid, Index, Data).

add(Pid, Index, Data) ->
        add(Pid, Index, Data, infinity).

add(Pid, Index, Data, Timeout) ->
        gen_server:call(Pid, {add, Index, Data}, Timeout).

delete(Index, Data) ->
        {ok, Pid} = plywood_work_sup:getWorker(),
        delete(Pid, Index, Data).

delete(Pid, Index, Data) ->
        delete(Pid, Index, Data, infinity).

delete(Pid, Index, Data, Timeout) ->
        gen_server:call(Pid, {delete, Index, Data}, Timeout).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
        {ok, Args}.

handle_call({add, Index, Data}, _From, State) ->
        ok = plywood:add(Index, jiffy:decode(Data, [return_maps])),
        {stop, normal, ok, State};
handle_call({delete, Index, Data}, _From, State) ->
        ok = plywood:delete(Index, jiffy:decode(Data, [return_maps])),
        {stop, normal, ok, State};
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

