-module(boss_load_controller).
-author('chan.sisowath@gmail.com').
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {mode, last, root, apps}).

%% code from project github.com/synrc/active 
%% modified for CB
start_link() -> 
    start_link([]).
start_link(Options) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], Options).

init(Opts) -> 
    lager:info("Starting boss_load..."),
    fs:subscribe(),
    Mode = proplists:get_value(mode, Opts, development), 
    CBApps = boss_env:get_env(applications, []),
    lager:info("Boss Apps under watch ~p",[CBApps]),
    erlang:process_flag(priority, low), 
    {ok, #state{mode=Mode, last=fresh, root=fs:path(), 
                     apps=[atom_to_list(App) || App <- CBApps]}}.

handle_call({add_app, App}, _From, #state{apps=Apps}=State) -> 
    NewApps = Apps ++ [atom_to_list(App)],
    {reply, ok, State#state{apps=NewApps}};
handle_call({del_app, App}, _From, #state{apps=Apps}=State) -> 
    NewApps = Apps -- [atom_to_list(App)],
    {reply, ok, State#state{apps=NewApps}};
handle_call({set_mode, Mode}, _From, State) -> 
    lager:info("boss_load set mode to ~p",[Mode]),
    {reply, {ok, Mode}, State#state{mode=Mode}};
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({_Pid, {fs,file_event}, {Path, Flags}}, #state{mode=Mode, root=Root} = State) ->
    Cur = path_shorten(filename:split(Root)),
    P = filename:split(Path),

    Result = case lists:prefix(Cur, P) of
        true ->
            Components = P -- Cur,
            case filelib:file_size(Path) of
                0 -> ignore;
                _ ->
                    %%error_logger:info_msg("~p event: ~p ~p", [Mode, Components, Flags]),
                    case Mode of
                        development ->
                            path_event(Components, Flags, State);
                        _ -> ignore
                    end
            end;
        false ->
            ok
    end,
    {noreply, State#state{last={event, Path, Flags, Result}}};

handle_info({load_ebin, App, Atom} = Event, State) -> 
    do_load_ebin(App, Atom),
    {noreply, State#state{last={do_load_ebin, Atom}}};
handle_info(Info, State) -> {noreply, State#state{last={unk, Info}}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


path_event(C, [E|_Events], #state{apps=CBApps}) 
  when 
      E =:= created; 
      E =:= modified; 
      E =:= renamed ->
    case path_filter(C) of 
        true  -> otp(C, CBApps); 
        false -> ignore 
    end;
path_event(C, [_E|Events], State) -> 
    path_event(C, Events, State);
path_event(_, [], _State) -> done.

otp(["deps",App|Rest], CBApps) -> app(deps, App,Rest, CBApps);
otp(["apps",App|Rest], CBApps) -> app(apps, App,Rest, CBApps);
otp([Some|Path], CBApps)       -> app(top, top(),[Some|Path], CBApps);
otp(_,_)                       -> ok.

app(deps, App, ["ebin",Module|_], _) -> load_ebin(App, Module);
app(_, App, ["ebin",Module|_], _)    -> load_ebin(App, Module);
app(_, _, ["priv","fdlink"++_], _) -> skip;
app(_, _, ["priv","mac"++_], _)    -> skip;
app(_, _, ["priv","windows"++_], _)-> skip;
app(_, _, ["priv","linux"++_], _)  -> skip;
app(Type, App, Path=["priv"|_], CBApps) -> 
    case hd(lists:reverse(Path)) of
        ".#" ++ _ -> skip; % mc temp files
        "#"  ++ _ -> skip;
        _      -> boss_load_lib:compile(Type, App, Path, CBApps)
    end;
app(Type, App,Path=["include"|_], CBApps)   -> boss_load_lib:compile(Type, App, Path, CBApps);
app(Type, App,Path=["src"|_], CBApps)       -> boss_load_lib:compile(Type, App, Path, CBApps);
app(Type, App,Path=["test"|_], CBApps)      -> boss_load_lib:compile(Type, App, Path, CBApps);
app(_, _, _, _)                             -> ok.

top() -> 
    lists:last(filename:split(filename:absname(""))).

load_ebin(App,EName) ->
    Tokens = string:tokens(EName, "."),
    case Tokens of
        [Name, "beam"] -> do_load_ebin(App, list_to_atom(Name));
        [Name, "bea#"] ->
            case monitor_handles_renames() of
                false ->
                    erlang:send_after(500, ?SERVER, {load_ebin, App, list_to_atom(Name)}),
                    delayed;
                true ->
                    ignored
            end;
        %%[Name, Smth] -> ok;
        _ ->
            error_logger:warning_msg("boss_load: unknown BEAM file: ~p", [EName]),
            ok
    end.

do_load_ebin(App, Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    code:load_binary(Module, Filename, Binary),
    lager:notice("Reloading [~s] ~p", [App, Module]),
    %%FIX for issue/323 and 322
    notify_boss(App, Module),
    %%FIX ME
    reloaded.

notify_boss(App, Module) ->
    gen_server:call(boss_web, {reloaded, App, Module}).
    
monitor_handles_renames([renamed|_]) -> true;
monitor_handles_renames([_|Events]) -> monitor_handles_renames(Events);
monitor_handles_renames([]) -> false.
monitor_handles_renames() ->
    case get(monitor_handles_renames) of
        undefined ->
            R = monitor_handles_renames(fs:known_events()),
            put(monitor_handles_renames, R),
            R;
        V -> V
    end.



%% ["a", "b", ".."] -> ["a"]
path_shorten(Coms) -> path_shorten_r(lists:reverse(Coms), [], 0).

path_shorten_r([".."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count + 1);
path_shorten_r(["."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count);
path_shorten_r([_C|Rest], Acc, Count) when Count > 0 -> path_shorten_r(Rest, Acc, Count - 1);
path_shorten_r([C|Rest], Acc, 0) -> path_shorten_r(Rest, [C|Acc], 0);
path_shorten_r([], Acc, _) -> Acc.

%
% Filters
%

path_filter(L) -> 
    not 
        lists:any(fun(E) -> 
                          not path_filter_dir(E) 
                  end,L) 
        andalso 
        path_filter_last(lists:last(L)).

path_filter_dir(".git") -> false;
path_filter_dir(".hg")  -> false;
path_filter_dir(".svn") -> false;
path_filter_dir("CVS")  -> false;
path_filter_dir("log")  -> false;
path_filter_dir("rel")  -> false;
path_filter_dir("_rel") -> false;
path_filter_dir(_)      -> true.

path_filter_last(".rebarinfo")     -> false;   % new rebars
path_filter_last("LICENSE")        -> false;
path_filter_last("4913 (deleted)") -> false;   % vim magical file
path_filter_last("4913")           -> false;
path_filter_last(Last)             -> 
    case Last of
        "#" ++ _ -> false; % emacs temp file
        Last -> check_extension(filename:extension(Last))
    end.
check_extension(".erl~") -> false;
check_extension(".erl#") -> false;
check_extension(".swp") -> false; % vi temp file
check_extension([]) -> false; % don't crash if file have no extension
check_extension(L) -> 
    case lists:last(L) of
        $~ -> false;
        $# -> false;
        _ -> true
    end.
