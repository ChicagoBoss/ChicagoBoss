{application, boss,
    [{description, "boss"},
    {vsn, "0.01"},
    {modules, [
        boss,
        boss_app,
        boss_controller
            ]},
    {registered, []},
    {mod, {boss_app, []}},
    {env, []},
    {applications, [kernel, stdlib, crypto]}]}.
