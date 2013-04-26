-module(boss_mail_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {driver, connection}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, boss_mail}, ?MODULE, Args, []).

init(Options) ->
    MailDriver = proplists:get_value(driver, Options, boss_mail_driver_smtp),
    {ok, Conn} = MailDriver:start(),
    {ok, #state{driver = MailDriver, connection = Conn}}.

handle_call({deliver, FromAddress, ToAddress, BodyFun, ResultFun}, _From, State) ->
    Driver = State#state.driver,
    {reply, Driver:deliver(State#state.connection, 
            FromAddress, ToAddress, BodyFun, ResultFun), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Driver = State#state.driver,
    Driver:stop(State#state.connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
