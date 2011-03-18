-module(hello_controller, [Req]).
-compile(export_all).

world('GET', []) ->
    Greetings = boss_db:find('greeting', []),
    case length(Greetings) of
        0 -> 
            {ok, SavedGreeting} = (greeting:new(id, "Boss says Hello!")):save(),
            {ok, [{greeting, SavedGreeting}]};
        N -> {ok, [{greeting, lists:nth(random:uniform(N), Greetings)}]}
    end.
