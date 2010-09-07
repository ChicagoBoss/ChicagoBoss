-module(boss_translator_controller).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dictionaries}).

start_link() ->
    gen_server:start_link({local, boss_translator}, ?MODULE, [], []).

init(_Options) ->
    DictionaryList = lists:map(fun(Lang) ->
                {Lang, dict:from_list(boss_lang:extract_po_strings(Lang))}
        end, boss_files:language_list()),
    {ok, #state{dictionaries = dict:from_list(DictionaryList)}}.

handle_call({lookup, Key, Locale}, _From, State) ->
    Return = case dict:find(Locale, State#state.dictionaries) of
        {ok, Dict} ->
            case dict:find(Key, Dict) of
                {ok, Trans} -> Trans;
                error -> undefined
            end;
        _ ->
            undefined
    end,
    {reply, Return, State};

handle_call({is_loaded, Locale}, _From, State) ->
    {reply, dict:is_key(Locale, State#state.dictionaries), State};

handle_call({reload, Locale}, _From, State) ->
    MessageDict = dict:from_list(boss_lang:extract_po_strings(Locale)),
    NewState = State#state{dictionaries = 
        dict:store(Locale, MessageDict, State#state.dictionaries)},
    {reply, ok, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
