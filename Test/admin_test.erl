-module(admin_test).
-compile(export_all).

root_test() ->
  boss_test:get_request("/", [],
    fun({Status, _, _, ParsedResponse}) ->
        [{Status =:= 200, "HTTP Status not OK"},
          {boss_test:find_link_with_text("\"An Evening With Chicago Boss\"", ParsedResponse) =/= undefined,
            "No link to the 'Evening'"}]
    end,
    [
      "Admin interface at /admin",
      fun(Response1) ->
          boss_test:follow_link("/admin", Response1,
            fun(Res) -> [boss_test:assert_http_ok(Res)] end,
            [
              "Click greeting",
              fun(Response2) ->
                  boss_test:follow_link("greeting", Response2,
                    fun(Res) -> [boss_test:assert_http_ok(Res)] end,
                    [
                      "Click + New greeting",
                      fun({_, GreetingUrl, _, _} = Response3) ->
                          boss_test:submit_form("new", [], Response3,
                            fun({Status, _, _, ParsedResponse} = Res) ->
                                [boss_test:assert_http_ok(Res),
                                  {boss_test:find_link_with_text("Back to the greeting list", ParsedResponse) =/= undefined,
                                    "No link back to the greeting list"}]
                            end,
                            [
                              "Submit valid greeting",
                              fun(Response4) ->
                                  boss_test:submit_form("create", [{"greeting_text", "Hello"}], Response4,
                                    fun(Res) -> [boss_test:assert_http_redirect(Res)] end,
                                    [
                                      "Redirect",
                                      fun({_, _, RespHeaders1, _}) ->
                                          NewGreetingUrl = proplists:get_value("Location", RespHeaders1),
                                          boss_test:get_request(NewGreetingUrl, [],
                                            fun({Status, _, RespHeaders, ParsedResponse} = Res) ->
                                                [boss_test:assert_http_ok(Res),
                                                  {boss_test:find_link_with_text(
                                                      "Back to the greeting list", ParsedResponse) =/= undefined,
                                                    "No link back to the greeting list"}]
                                            end,
                                            [
                                              "Delete record",
                                              fun(Response6) ->
                                                  boss_test:submit_form("delete", [], Response6,
                                                    fun(Res) -> [boss_test:assert_http_ok(Res)] end,
                                                    [
                                                      "Really delete",
                                                      fun(Response7) ->
                                                          boss_test:submit_form("delete", [], Response7,
                                                            fun({_, _, Headers8, _} = Res) -> 
                                                                Redirect = proplists:get_value("Location", Headers8),
                                                                [boss_test:assert_http_redirect(Res),
                                                                  {Redirect =:= GreetingUrl, 
                                                                    "Did not redirect to Greeting list"}] 
                                                            end,
                                                            []
                                                          )
                                                      end
                                                    ]
                                                  )
                                              end
                                            ]
                                          )
                                      end
                                    ]
                                  )
                              end,
                              "Submit invalid greeting",
                              fun(Response4) ->
                                  boss_test:submit_form("create", [{"greeting_text", "Hi"}], Response4,
                                    fun({Status, _, RespHeaders, ParsedResponse} = Res) ->
                                        [boss_test:assert_http_ok(Res)]
                                    end,
                                    [
                                    ]
                                  )
                              end
                            ]
                          )
                      end
                    ]
                  )
              end
            ]
          )
      end,
      "EDoc at /doc",
      fun(Response1) ->
          boss_test:follow_link("/doc", Response1,
            fun({Status, _, RespHeaders, ParsedResponse}) ->
                [{Status =:= 200, 
                    "HTTP Status not OK"},
                  {boss_test:find_link_with_text("Function Index", ParsedResponse) =/= undefined,
                    "No link to function index"},
                  {boss_test:find_link_with_text("id/0*", ParsedResponse) =/= undefined,
                    "No link to id/0*"}
                ]
            end,
            []
          )
      end
    ]
  ).

