-module(hello_controller).
-compile(export_all).

world(Req) ->
    {ok, [{world, "World"}]}.
