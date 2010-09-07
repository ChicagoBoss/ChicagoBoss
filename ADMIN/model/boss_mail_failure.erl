-module(boss_mail_failure, [Id, CreationTime, FromAddress, ToAddress, BodyFun, ErrorMessage]).
-compile(export_all).

body_fun() ->
    binary_to_term(BodyFun).

error_message() ->
    binary_to_term(ErrorMessage).
