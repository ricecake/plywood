-module(plywood_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([add/2, add/3, add/4]).
-export([delete/2, delete/3, delete/4]).
-export([lookup/3, lookup/4, lookup/5]).

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

lookup(Index, Path, Req) ->
	{ok, Pid} = plywood_work_sup:getWorker(),
	lookup(Pid, Index, Path, Req).

lookup(Pid, Index, Path, Req) ->
	lookup(Pid, Index, Path, Req, infinity).

lookup(Pid, Index, Path, Req, Timeout) ->
	case gen_server:call(Pid, {lookup, Index, Path}, Timeout) of
		{ok, Data} ->	
			Result = plywood:export(
				lists:reverse(Path),
				Data
			),
			cowboy_req:reply(
				200,
				[{
					<<"content-type">>,
					<<"text/json; charset=utf-8">>
				}],
				jiffy:encode(Result),
				Req
			);
		{false, Error} ->
			cowboy_req:reply(
				500,
				[],
				Error,
				Req
			)
	end.


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
handle_call({lookup, Index, Path}, _From, State) ->
	Response = try plywood:lookup(Index, Path) of
		Data               -> {ok, Data}
	catch
		_Exception:_Reason -> {false, <<"Error">>}
	end,
	{reply, Response, State};
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

