{application, boss,
    [{description, "boss"},
    {vsn, "0.01"},
    {modules, [
        boss,
        boss_app,
        boss_controller,
        boss_db,
        boss_db_controller,
        boss_db_sup
            ]},
    {registered, []},
    {mod, {boss_app, []}},
    {env, []},
    {applications, [kernel, stdlib, crypto]}]}.
