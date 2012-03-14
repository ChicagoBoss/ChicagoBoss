-module(ybed).
-include("../include/yaws_api.hrl").
-compile(export_all).

out(Req) -> 
	boss_web_controller:handle_request(Req, yaws_request_bridge, yaws_response_bridge).
	
start(Port) ->
    {ok, spawn(?MODULE, run, [Port])}.

run(Port) ->
    Id = "embedded",
    GconfList = [{id, Id}],
    Docroot = "./priv/static",
    SconfList = [{port, Port},
                 {servername, "localhost"},
                 {listen, {0,0,0,0}},
                 {appmods, [{"/", ybed}]}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
