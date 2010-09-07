-module(mail_test).
-export([start/0]).

start() ->
  boss_mail:send(test_message, ["root@chicagoboss.org", "emmiller@gmail.com", "You've won!"]),
  boss_test:read_email("emmiller@gmail.com", "You've won!",
    [
      fun boss_assert:email_is_multipart/1,
      fun(Email) -> boss_assert:link_with_text("Click here", Email) end,
      fun(Email) -> boss_assert:from_header("root@chicagoboss.org", Email) end
    ], 
    [
      "Follow link in email",
      fun(Email) ->
          boss_test:follow_link("Click here", Email,
            [
              fun boss_assert:http_ok/1
            ], []) end
    ]).
