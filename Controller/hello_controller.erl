-module(hello_controller).
-compile(export_all).

world(Req) ->
    {ok, [{greeting, greeting:new(id, "Hello, World!")}]}.
