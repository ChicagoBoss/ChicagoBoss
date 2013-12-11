-module(boss_web_controller_handle_request).

-export([handle_request/3]).


-include("boss_web.hrl").


%% TODO REFACTOR AND TEST
handle_request(Req, RequestMod, ResponseMod) ->
	LoadedApplications = boss_web:get_all_applications(),
	Request = simple_bridge:make_request(RequestMod, Req),
	FullUrl = Request:path(),
	case boss_web_controller_util:find_application_for_path(Request:header(host), FullUrl, LoadedApplications) of
		undefined ->
			Response = simple_bridge:make_response(ResponseMod, {Req, undefined}),
			Response1 = (Response:status_code(404)):data(["No application configured at this URL"]),
			Response1:build_response();
		App ->
			BaseURL = boss_web:base_url(App),
			DocRoot = boss_files:static_path(App),
			StaticPrefix = boss_web:static_prefix(App),
			Url = lists:nthtail(length(BaseURL), FullUrl),
			Response = simple_bridge:make_response(ResponseMod, {Req, DocRoot}),
			case lists:member(Url, boss_env:get_env(App, static_files, ["/favicon.ico", "/apple-touch-icon.png", "/robots.txt"])) of
				true ->
					(Response:file(Url)):build_response();
				false ->
					case string:substr(Url, 1, length(StaticPrefix)) of
						StaticPrefix ->
							[$/|File] = lists:nthtail(length(StaticPrefix), Url),
							Response2 = case boss_env:boss_env() of
								development ->
									Response:header("Cache-Control", "no-cache");
								_ ->
									Response
							end,
							Response3 = Response2:file([$/|File]),
							Response3:build_response();
						_ ->
							build_dynamic_response(App, Request, Response, Url)
					end
			end
	end.

%% TODO: Refactor
build_dynamic_response(App, Request, Response, Url) ->
    Mode                = boss_web_controller_util:execution_mode(App),
    AppInfo		= boss_web:application_info(App),
    TranslatorPid	= boss_web:translator_pid(App),
    RouterPid		= boss_web:router_pid(App),
    ControllerList	= boss_files:web_controller_list(App),
    {Time, {StatusCode, Headers, Payload}} = timer:tc(?MODULE, process_request, [
            AppInfo#boss_app_info{
                translator_pid		= TranslatorPid,
                router_pid		= RouterPid,
                controller_modules	= ControllerList
            },
            Request, Mode, Url]),
    ErrorFormat		= "~s ~s [~p] ~p ~pms",
    RequestMethod	= Request:request_method(),
    FullUrl		= Request:path(),
    ErrorArgs		= [RequestMethod, FullUrl, App, StatusCode, Time div 1000],
    log_status_code(StatusCode, ErrorFormat, ErrorArgs),
    Response1		= (Response:status_code(StatusCode)):data(Payload),
    Response2		= lists:foldl(fun({K, V}, Acc) -> Acc:header(K, V) end, Response1, Headers),
    handle_response(Request, Payload, RequestMethod, Response2).

handle_response(Request, Payload, RequestMethod, Response2) ->
    case Payload of
        {stream, Generator, Acc0} ->
            TransferEncoding = 
		case Request:protocol_version() of 
		    {1, 1} -> chunked;
		    _ -> identity
		end,
            Response3 = Response2:data(chunked),
            Response3:build_response(),
            process_stream_generator(Request, TransferEncoding, RequestMethod, Generator, Acc0);
        _ ->
            (Response2:data(Payload)):build_response()
    end.

log_status_code(StatusCode, ErrorFormat, ErrorArgs) ->
    case StatusCode of
        500 -> error_logger:error_msg(ErrorFormat, ErrorArgs);
        404 -> error_logger:warning_msg(ErrorFormat, ErrorArgs);
        _   -> error_logger:info_msg(ErrorFormat, ErrorArgs)
    end.

process_stream_generator(_Req, _TransferEncoding, 'HEAD', _Generator, _Acc) ->
    ok;
process_stream_generator(Req, chunked, Method, Generator, Acc) ->
    case Generator(Acc) of
        {output, Data, Acc1} ->
            case iolist_size(Data) of
                0 -> ok;
                Length ->
                    Chunk = [io_lib:format("~.16b\r\n", [Length]), Data, <<"\r\n">>],
                    ok = mochiweb_socket:send(Req:socket(), Chunk)
            end,
            process_stream_generator(Req, chunked, Method, Generator, Acc1);
        done -> ok = mochiweb_socket:send(Req:socket(), ["0\r\n\r\n"])
    end;
process_stream_generator(Req, identity, Method, Generator, Acc) ->
    case Generator(Acc) of
        {output, Data, Acc1} ->
            mochiweb_socket:send(Req:socket(), Data),
            process_stream_generator(Req, identity, Method, Generator, Acc1);
        done -> ok
    end.