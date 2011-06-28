-module(boss_translator_controller).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {strings, blocks}).

start_link() ->
    gen_server:start_link({local, boss_translator}, ?MODULE, [], []).

init(_Options) ->
    StringDictionaryList = lists:map(fun(Lang) ->
                {Lang, dict:from_list(boss_lang:extract_po_strings(Lang))}
        end, boss_files:language_list()),
    BlockDictionaryList = lists:map(fun(Lang) ->
                {Lang, dict:from_list(boss_lang:extract_po_blocks(Lang, comment))}
        end, boss_files:language_list()),
    {ok, #state{strings = dict:from_list(StringDictionaryList), blocks = dict:from_list(BlockDictionaryList)}}.

handle_call({lookup, Key, Locale}, _From, State) when is_atom(Key) ->
    Return = case dict:find(Locale, State#state.blocks) of
        {ok, Dict} ->
            case dict:find(atom_to_list(Key), Dict) of
                {ok, Trans} -> Trans;
                error -> undefined
            end;
        _ ->
            undefined
    end,
    {reply, Return, State};
handle_call({lookup, Key, Locale}, _From, State) ->
    Return = case dict:find(Locale, State#state.strings) of
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
    {reply, dict:is_key(Locale, State#state.strings), State};

handle_call({reload, Locale}, _From, State) ->
    StringDict = dict:from_list(boss_lang:extract_po_strings(Locale)),
    BlockDict = dict:from_list(boss_lang:extract_po_blocks(Locale, comment)),
    NewState = State#state{
        strings = dict:store(Locale, StringDict, State#state.strings),
        blocks = dict:store(Locale, BlockDict, State#state.blocks)
    },
    {reply, ok, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
