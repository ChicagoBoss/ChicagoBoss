-module(admin_test).
-compile(export_all).

start() ->
  boss_web_test:get_request("/hello/world", [],
    [ fun boss_assert:http_ok/1,
      fun(Res) -> boss_assert:link_with_text("\"An Evening With Chicago Boss\"", Res) end,
      fun(Res) -> boss_assert:link_with_text("chicagoboss.org", Res) end,
      fun(Res) -> boss_assert:link_with_text("Chicago Boss Google Group", Res) end
    ],
    [ "Admin interface at /admin",
      fun(Response1) ->
          boss_web_test:follow_link("/admin", Response1,
            [ fun boss_assert:http_ok/1,
              fun(Res) -> boss_assert:tag_with_text("title", "Chicago Boss Admin Interface", Res) end
            ],
            [ "Click greeting",
              fun(Response2) ->
                  boss_web_test:follow_link("greeting", Response2,
                    [ fun boss_assert:http_ok/1,
                      fun (Res) -> boss_assert:link_with_text("Documentation for the greeting model", Res) end
                    ],
                    [ "Click + New greeting",
                      fun({_, GreetingUrl, _, _} = Response3) ->
                          boss_web_test:submit_form("new", [], Response3,
                            [ fun boss_assert:http_ok/1,
                              fun(Res) -> boss_assert:link_with_text("Back to the greeting list", Res) end
                            ],
                            [ "Submit valid greeting",
                              fun(Response4) ->
                                  ValidGreetingText = "Hello",
                                  boss_web_test:submit_form("create", [{"greeting_text", ValidGreetingText}], Response4,
                                    [ fun boss_assert:http_redirect/1 ],
                                    [ "Redirect",
                                      fun(Response5) ->
                                          boss_web_test:follow_redirect(Response5,
                                            [ fun boss_assert:http_ok/1,
                                              fun(Res) -> boss_assert:link_with_text("Back to the greeting list", Res) end
                                            ],
                                            [ "Delete record",
                                              fun(Response6) ->
                                                  boss_web_test:submit_form("delete", [], Response6,
                                                    [ fun boss_assert:http_ok/1 ],
                                                    [ "Really delete",
                                                      fun(Response7) ->
                                                          boss_web_test:submit_form("delete", [], Response7,
                                                            [ fun boss_assert:http_redirect/1,
                                                              fun(Res) -> boss_assert:location_header(GreetingUrl, Res) end
                                                            ], []) end ]) end,
                                              "Back to greeting list",
                                              fun(Response6) ->
                                                  boss_web_test:follow_link("Back to the greeting list", Response6,
                                                    [ fun boss_assert:http_ok/1,
                                                      fun(Res) -> boss_assert:tag_with_text("td", ValidGreetingText, Res) end
                                                    ], []) end ]) end ]) end,
                              "Submit invalid greeting",
                              fun(Response4) ->
                                  boss_web_test:submit_form("create", [{"greeting_text", "Hi"}], Response4,
                                    [ fun boss_assert:http_ok/1,
                                      fun(Res) -> boss_assert:tag_with_text("li", "Greeting must be at least 4 characters long!", Res) end
                                    ], []) end ]) end ]) end ]) end,
      "EDoc at /doc",
      fun(Response1) ->
          boss_web_test:follow_link("/doc", Response1,
            [
              fun boss_assert:http_ok/1,
              fun(Res) -> boss_assert:link_with_text("Function Index", Res) end,
              fun(Res) -> boss_assert:link_with_text("Function Details", Res) end,
              fun(Res) -> boss_assert:link_with_text("attribute_names/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("attributes/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("attributes/1*", Res) end,
              fun(Res) -> boss_assert:link_with_text("belongs_to/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("belongs_to_names/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("greeting_text/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("greeting_text/1*", Res) end,
              fun(Res) -> boss_assert:link_with_text("id/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("id/1*", Res) end,
              fun(Res) -> boss_assert:link_with_text("save/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("validate/0*", Res) end,
              fun(Res) -> boss_assert:link_with_text("validation_tests/0*", Res) end
            ], []) 
      end
%      ,
%
%      "Check for confirmation email",
%      fun(Response1) ->
%          boss_web_test:read_email("test@test.com", "Confirmation email",
%            [
%              fun(Email2) -> boss_assert:link_with_text("Click here to confirm", Email) end
%            ],
%            [
%              "Click confirmation link",
%              fun(Email2) ->
%                  boss_web_test:follow_link("Click here to confirm", Email,
%                    [
%                      fun(Response3) -> boss_assert:tag_with_text(
%                    ],
%      end
    ]).
