-module(boss_controller).
-export([mochiweb_request/1, start/0, start/1, stop/0, render_view/2, process_request/1]).

start() ->
    start([]).

start(Config) ->
    {ok, DBPort} = application:get_env(db_port),
    {ok, DBDriver} = application:get_env(db_driver),
    {ok, DBHost} = application:get_env(db_host),
    {ok, LogFile} = application:get_env(log_file),
    boss_db:start([ {port, DBPort}, {driver, DBDriver}, {host, DBHost} ]),
    case disk_log:open([{name, boss_error_log}, {file, LogFile}]) of
        {ok, boss_error_log} -> ok;
        {repaired,boss_error_log,_,_} -> repaired;
        Error -> io:format("disk_log:open error ~p~n",[Error])
    end,

    load_dir(controller_path(), fun compile_controller/1),
    load_dir(model_path(), fun compile_model/1),
    mochiweb_http:start([{loop, fun(Req) -> mochiweb_request(Req) end} | Config]).

stop() ->
    disk_log:close(boss_error_log),
    boss_db:stop(),
    mochiweb_http:stop().

mochiweb_request(Req) ->
    DocRoot = "./static",
    Request = simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}),
    case Request:path() of
        "/static/"++File -> 
            Response = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
            (Response:file([$/|File])):build_response();
        _ -> 
            {StatusCode, Headers, Payload} = process_request(Request),
            Response = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
            Response1 = (Response:status_code(StatusCode)):data(Payload),
            Response2 = lists:foldl(fun({K, V}, Acc) -> Acc:header(K, V) end, Response1, Headers),
            Response2:build_response()
    end.

process_request(Req) ->
    Result = case parse_path(Req:path()) of
        {ok, {Controller, Action, Tokens}} ->
            trap_load_and_execute({Controller, Action, Tokens}, Req);
        Else ->
            Else
        end,
    process_result(Result).

parse_path("/") ->
    {ok, {Controller, Action}} = application:get_env(front_page),
    {ok, {Controller, Action, []}};
parse_path("/" ++ Url) ->
    Tokens = string:tokens(Url, "/"),
    case length(Tokens) of
        1 ->
            Controller = hd(Tokens),
            {ok, DefaultAction} = application:get_env(default_action),
            {ok, AllDefaultActions} = application:get_env(default_actions),
            ThisAction = proplists:get_value(Controller, AllDefaultActions, DefaultAction),
            {ok, {Controller, ThisAction, []}};
        N when N >= 2 ->
            {ok, {lists:nth(1, Tokens), 
                    lists:nth(2, Tokens),
                    lists:nthtail(2, Tokens)}};
        _ ->
            {not_found, "File not found"}
    end;
parse_path(_) ->
    {not_found, "File not found"}.

process_result({error, Payload}) ->
    disk_log:balog(boss_error_log, list_to_binary(format_now(erlang:now()) ++ 
            " Error : "++io_lib:print(Payload)++"\n\n")),
    {500, [{"Content-Type", "text/html"}], "Error: <pre>" ++ io_lib:print(Payload) ++ "</pre>"};
process_result({not_found, Payload}) ->
    {404, [{"Content-Type", "text/html"}], Payload};
process_result({redirect, Where}) ->
    process_result({redirect, Where, []});
process_result({redirect, "http://"++Where, Headers}) ->
    process_result({redirect, "/"++string:join(tl(string:tokens(Where, "/")), "/"), Headers});
process_result({redirect, Where, Headers}) ->
    {302, [{"Location", Where}, {"Cache-Control", "no-cache"}|Headers], ""};
process_result({ok, Payload, Headers}) ->
    {200, proplists:delete("Content-Type", Headers) ++ 
        [{"Content-Type", proplists:get_value("Content-Type", Headers, "text/html")}], 
        Payload};
process_result({ok, Payload}) ->
    {200, [{"Content-Type", "text/html"}], Payload}.

trap_load_and_execute(Arg1, Arg2) ->
    case catch load_and_execute(Arg1, Arg2) of
        {'EXIT', Reason} ->
            {error, Reason};
        Ok ->
            Ok
    end.

load_and_execute({"doc", ModelName, _}, _Req) ->
    case load_dir(model_path(), fun compile_model/1) of
        {ok, _} ->
            Model = list_to_atom(ModelName),
            {Model, Edoc} = boss_record_compiler:edoc_module(
                model_path(ModelName++".erl"), [{private, true}]),
            {ok, edoc:layout(Edoc)};
        Error ->
            Error
    end;
load_and_execute({Controller, _, _} = Location, Req) ->
    case load_dir(controller_path(), fun compile_controller/1) of
        {ok, Controllers} ->
            case lists:member(Controller ++ "_controller", Controllers) of
                true ->
                    case load_dir(model_path(), fun compile_model/1) of
                        {ok, _} ->
                            execute_action(Location, Req);
                        Else ->
                            Else
                    end;
                false ->
                    render_view(Location)
            end;
        Else ->
            Else
    end.

execute_action(Location, Req) ->
    execute_action(Location, Req, []).

execute_action({Controller, Action}, Req, LocationTrail) ->
    execute_action({Controller, Action, []}, Req, LocationTrail);
execute_action({Controller, Action, Tokens}, Req, LocationTrail) when is_atom(Action) ->
    execute_action({Controller, atom_to_list(Action), Tokens}, Req, LocationTrail);
execute_action({Controller, Action, Tokens} = Location, Req, LocationTrail) ->
    case lists:member(Location, LocationTrail) of
        true ->
            {error, "Circular redirect!"};
        _ ->
            % do not convert a list to an atom until we are sure the controller/action
            % pair exists. this prevents a memory leak due to atom creation.
            Module = list_to_atom(lists:concat([Controller, "_controller"])),
            ControllerInstance = Module:new(Req),
            ExportStrings = lists:map(
                fun({Function, Arity}) -> {atom_to_list(Function), Arity} end,
                Module:module_info(exports)),
            case lists:member({Action, 3}, ExportStrings) of
                true ->
                    ActionAtom = list_to_atom(Action),
                    process_action_result({Location, Req, LocationTrail}, 
                        ControllerInstance:ActionAtom(Req:request_method(), Tokens));
                false -> 
                    case lists:member({Action, 4}, ExportStrings) of
                        true -> 
                            case ControllerInstance:'_auth'(Action) of
                                {ok, Info} ->
                                    ActionAtom = list_to_atom(Action),
                                    process_action_result({Location, Req, LocationTrail}, 
                                        ControllerInstance:ActionAtom(Req:request_method(), Tokens, Info));
                                Other -> Other
                            end;
                        _ -> render_view(Location)
                    end
            end
    end.

process_action_result(Info, ok) ->
    process_action_result(Info, {ok, []});
process_action_result(Info, {ok, Data}) ->
    process_action_result(Info, {ok, Data, []});
process_action_result({Location, _, _}, {ok, Data, Headers}) ->
    render_view(Location, Data, Headers);

process_action_result(Info, {render_other, OtherLocation}) ->
    process_action_result(Info, {render_other, OtherLocation, []});
process_action_result(_, {render_other, OtherLocation, Data}) ->
    render_view(OtherLocation, Data);

process_action_result({_, Req, LocationTrail}, {action_other, OtherLocation}) ->
    execute_action(OtherLocation, Req, [OtherLocation | LocationTrail]);
process_action_result(_, Else) ->
    Else.

compile_controller(ModulePath) ->
    CompileResult = compile:file(filename:rootname(ModulePath),
        [{outdir, filename:join([root_dir(), "ebin"])}, return_errors]),
    case CompileResult of
        {ok, Module} ->
            code:purge(Module),
            {module, Module} = code:load_file(Module),
            ok;
        {error, ErrorList, WarningList}  ->
            {error, ["Failed to compile " ++ ModulePath ++ ". ", ErrorList, WarningList]}
    end.

compile_view_erlydtl(Controller, Template) ->
    erlydtl_compiler:compile(
        view_path(Controller, Template), 
        view_module(Controller, Template), 
        [{doc_root, view_path()}, {compiler_options, []}]).

compile_view_etcher(Controller, Template) ->
    case file:read_file(view_path(Controller, Template)) of
        {ok, Binary} ->
            etcher:compile(Binary);
        Err ->
            Err
    end.

compile_model(ModulePath) ->
    boss_record_compiler:compile(ModulePath).

load_dir(Dir, Compiler) ->
    {ok, Files} = file:list_dir(Dir),
    {ModuleList, ErrorList} = lists:foldl(fun
            ("."++_, Acc) ->
                Acc;
            (File, {Modules, Errors}) ->
                case lists:suffix(".erl", File) of
                    true ->
                        Module = filename:basename(File, ".erl"),
                        AbsPath = filename:join([Dir, File]),
                        case module_older_than(list_to_atom(Module), [AbsPath]) of
                            true ->
                                case Compiler(AbsPath) of
                                    ok ->
                                        {[Module|Modules], Errors};
                                    {error, Error} ->
                                        {Modules, [Error | Errors]};
                                    {error, NewErrors, _NewWarnings} when is_list(NewErrors) ->
                                        {Modules, NewErrors ++ Errors}
                                end;
                            _ ->
                                {[Module|Modules], Errors}
                        end;
                    _ ->
                        {Modules, Errors}
                end
        end, {[], []}, Files),
    case length(ErrorList) of
        0 ->
            {ok, ModuleList};
        _ ->
            {error, ErrorList}
    end.

render_view(Location) ->
    render_view(Location, []).

render_view(Location, Variables) ->
    render_view(Location, Variables, []).

render_view({Controller, Template, _} = Location, Variables, Headers) ->
    Module = view_module(Controller, Template),
    Result = case module_is_loaded(Module) of
        true ->
            case module_older_than(Module, lists:map(fun
                            ({File, _CheckSum}) -> 
                                File;
                            (File) ->
                                File
                    end, [Module:source() | Module:dependencies()])) of
                true ->
                    compile_view_erlydtl(Controller, Template);
                false ->
                    ok
            end;
        false ->
            compile_view_erlydtl(Controller, Template)
    end,
    case Result of
        ok ->
            case Module:render(Variables) of
                {ok, Payload} ->
                    {ok, Payload, Headers};
                Err ->
                    Err
            end;
        _ -> 
            case render_view_etcher(Location, Variables) of
                {ok, Payload} ->
                    {ok, Payload, Headers};
                Err ->
                    Err
            end
    end.


render_view_etcher({Controller, Template, _}, Variables) ->
    case compile_view_etcher(Controller, Template) of
        {ok, CompiledTemplate} ->
            RenderOpts = [{template_loaders, [{file, [view_path()]}]}],
            {ok, etcher:render(CompiledTemplate, Variables, RenderOpts)};
        Err ->
            Err
    end.


module_is_loaded(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            true;
        _ ->
            false
    end.

module_older_than(Module, Files) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, Loaded} ->
            module_older_than(Loaded, Files);
        _ ->
            case code:load_file(Module) of
                {module, _} ->
                    case code:is_loaded(Module) of
                        {file, Loaded} ->
                            module_older_than(Loaded, Files)
                    end;
                {error, _} ->
                    true
            end
    end;

module_older_than(Module, Files) when is_list(Module) ->
    case filelib:last_modified(Module) of
        0 ->
            true;
        CompileDate ->
            module_older_than(CompileDate, Files)
    end;

module_older_than(_Date, []) ->
    false;

module_older_than(CompileDate, [File|Rest]) ->
    CompileSeconds = calendar:datetime_to_gregorian_seconds(CompileDate),
    ModificationSeconds = calendar:datetime_to_gregorian_seconds(
        filelib:last_modified(File)),
    (ModificationSeconds >= CompileSeconds) orelse module_older_than(CompileDate, Rest).

view_module(Controller, Template) ->
    list_to_atom(lists:concat([Controller, "_view_", Template])).

root_dir() -> filename:join([filename:dirname(code:which(?MODULE)), ".."]).

view_path() -> filename:join([root_dir(), "View"]).
view_path(Controller) -> filename:join([view_path(), Controller]).
view_path(Controller, Template) -> filename:join([view_path(Controller), lists:concat([Template, ".html"])]).

model_path() -> filename:join([root_dir(), "Model"]).
model_path(Model) -> filename:join([model_path(), Model]).

controller_path() -> filename:join([root_dir(), "Controller"]).

format_now(Time) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Time),
    integer_to_list(Year) ++ "." ++ integer_to_list(Month) ++ "." ++ integer_to_list(Day) ++
        " "++integer_to_list(Hour)++":"++integer_to_list(Minute)++":"++integer_to_list(Second).
