-module(boss_modtime_srv).


-behaviour(gen_server).

%% API
-export([start/0, time/1, set/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).


time(Module) ->
	case ets:lookup(?MODULE, Module) of
		[] -> 0;
		[{_, T}] -> T
	end.
set({Module, Time}) ->
	gen_server:cast(?SERVER, {set, Module, Time}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
	erlang:display(boss_modtime_srv),
	Tab = ets:new(?MODULE, [named_table, protected, {keypos, 1}, {read_concurrency, true}]),
	case catch file:consult(".cb/modules") of
		{ok, [L]} when is_list(L) -> ets:insert(Tab, L);
		_ ->
			ok
	end,
	{ok, #state{tab = Tab}}.



handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast({set, Module, Time}, #state{tab = Tab} = State) ->
	ets:insert(Tab, {Module, Time}),
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
