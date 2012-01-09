-module(boss_session_controller).

-behaviour(gen_leader).

-export([start_link/0, start_link/1]).

-export([init/1,
         handle_cast/3,
         handle_call/4,
         handle_info/2,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/3,
         surrendered/3,
         from_leader/3,
         code_change/4,
         terminate/2]).

-define(SERVER, boss_session).
-define(INFO(Message, Args), error_logger:info_msg(lists:flatten(io_lib:format(Message, Args))++"~n")).

-record(state, {
        adapter, 
        connection
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    Nodes = ['cb1@127.0.0.1', 'cb2@127.0.0.1'],
    Opts = [],
    gen_leader:start_link(?SERVER, Nodes, [Opts], ?MODULE, Args, []).

init(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_session_adapter_mock),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    {ok, Conn} = Adapter:start(Options),
    {ok, #state{adapter = Adapter, connection = Conn }}.

elected(State, _Election, undefined) ->
    ?INFO("~p elected",[node()]),
    {ok, [], State};

elected(State, _Election, _Node) ->
    ?INFO("~p elected",[node()]),
    {reply, [], State}.

handle_call({new_session, Cookie}, _From, State, _Election) ->
    NewSessionID = new_session(State#state.adapter, State#state.connection, Cookie),
    gen_leader:leader_cast(?SERVER, {new_session, Cookie, NewSessionID}),
    {reply, NewSessionID, State};

handle_call({get_session_data, Sid}, _From, State, _Election) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:lookup_session(Conn, Sid), State};

handle_call({get_session_data, Sid, Key}, _From, State, _Election) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:lookup_session_value(Conn, Sid, Key), State};

handle_call({set_session_data, Sid, Key, Value}, _From, State, _Election) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:set_session_value(Conn, Sid, Key, Value), State};

handle_call({delete_session, SessionID}, _From, State, _Election) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete_session(Conn, SessionID), State};

handle_call({remove_session_data, Sid, Key}, _From, State, _Election) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    {reply, Adapter:delete_session_value(Conn, Sid, Key), State}.

handle_leader_call(_Msg, _From, State, _Election) ->
    {reply, ok, State}.

handle_leader_cast({new_session, Cookie, SessionID}, State, _Election) ->
    NewSessionID = new_session(State#state.adapter, State#state.connection,
                               Cookie, SessionID),
    {ok, {new_session, Cookie, NewSessionID}, State};
handle_leader_cast(_Msg, State, _Election) ->
    {ok, State}.

handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

from_leader({new_session, Cookie, SessionID}, State, _Election) ->
    new_session(State#state.adapter, State#state.connection,
                               Cookie, SessionID),
    {ok, State};
from_leader(_Synch, State, _Election) ->
    {ok, State}.

handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

surrendered(State, _Synch, _Election) ->
    {ok, State}.

terminate(_Reason, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Adapter:stop(Conn).

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

new_session(Adapter, Conn, Cookie) ->
    new_session(Adapter, Conn, Cookie, generate_session_id()).

new_session(Adapter, Conn, Cookie, SessionID) ->
    case Adapter:session_exists(Conn, Cookie) of
        true ->
            Cookie;
        false ->
            Adapter:create_session(Conn, SessionID, []),
            SessionID
    end.

generate_session_id() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    lists:flatten(list_to_hex(Sha_list)).
%% Convert Integer from the SHA to Hex
list_to_hex(L)->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 -> 
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
