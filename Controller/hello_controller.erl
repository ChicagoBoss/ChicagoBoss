-module(hello_controller, [Req]).
-compile(export_all).

world('GET', []) ->
    Greetings = boss_db:find('greeting', [], 1000),
    case length(Greetings) of
        0 -> {ok, [{greeting, (greeting:new(id, "Boss says Hello!")):save()}]};
        N -> {ok, [{greeting, lists:nth(random:uniform(N), Greetings)}]}
    end.
