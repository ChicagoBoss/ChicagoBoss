-module(hello_controller, [Req]).
-compile(export_all).

world('GET', []) ->
    {ok, [{greeting, greeting:new(id, "Boss says Hello!")}]}.
