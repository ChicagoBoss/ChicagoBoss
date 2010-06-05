-module(admin_test).
-compile(export_all).

root_test() ->
  boss_test:get_request("/", [],
    fun(Res) -> [
          boss_assert:http_ok(Res),
          boss_assert:link_with_text("\"An Evening With Chicago Boss\"", Res),
          boss_assert:link_with_text("chicagoboss.org", Res),
          boss_assert:link_with_text("Chicago Boss Google Group", Res)
        ]
    end,
    [
      "Admin interface at /admin",
      fun(Response1) ->
          boss_test:follow_link("/admin", Response1,
            fun(Res) -> [
                  boss_assert:http_ok(Res),
                  boss_assert:tag_with_text("title", "Chicago Boss Admin Interface", Res)
                ] 
            end,
            [
              "Click greeting",
              fun(Response2) ->
                  boss_test:follow_link("greeting", Response2,
                    fun(Res) -> [
                          boss_assert:http_ok(Res),
                          boss_assert:link_with_text("Documentation for the greeting model", Res)
                        ] end,
                    [
                      "Click + New greeting",
                      fun({_, GreetingUrl, _, _} = Response3) ->
                          boss_test:submit_form("new", [], Response3,
                            fun(Res) -> [
                                  boss_assert:http_ok(Res),
                                  boss_assert:link_with_text("Back to the greeting list", Res)
                                ]
                            end,
                            [
                              "Submit valid greeting",
                              fun(Response4) ->
                                  boss_test:submit_form("create", [{"greeting_text", "Hello"}], Response4,
                                    fun(Res) -> [boss_assert:http_redirect(Res)] end,
                                    [
                                      "Redirect",
                                      fun(Response5) ->
                                          boss_test:follow_redirect(Response5,
                                            fun(Res) -> [
                                                  boss_assert:http_ok(Res),
                                                  boss_assert:link_with_text("Back to the greeting list", Res)
                                                ]
                                            end,
                                            [
                                              "Delete record",
                                              fun(Response6) ->
                                                  boss_test:submit_form("delete", [], Response6,
                                                    fun(Res) -> [boss_assert:http_ok(Res)] end,
                                                    [
                                                      "Really delete",
                                                      fun(Response7) ->
                                                          boss_test:submit_form("delete", [], Response7,
                                                            fun(Res) -> [
                                                                  boss_assert:http_redirect(Res),
                                                                  boss_assert:redirect_location(GreetingUrl, Res)
                                                                ]
                                                            end, []) end ]) end ]) end ]) end,
                              "Submit invalid greeting",
                              fun(Response4) ->
                                  boss_test:submit_form("create", [{"greeting_text", "Hi"}], Response4,
                                    fun(Res) ->
                                        [boss_assert:http_ok(Res)]
                                    end, []) end ]) end ]) end ])
      end,
      "EDoc at /doc",
      fun(Response1) ->
          boss_test:follow_link("/doc", Response1,
            fun(Res) -> [
                  boss_assert:http_ok(Res),
                  boss_assert:link_with_text("Function Index", Res),
                  boss_assert:link_with_text("id/0*", Res)
                ] 
            end, []) end ]).
