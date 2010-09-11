{application, boss,
    [{description, "Chicago Boss"},
    {vsn, "0.4.1"},
    {modules, [
        boss,
        boss_app,
        boss_web_controller,
        boss_db,
        boss_db_controller,
        boss_db_sup
            ]},
    {registered, []},
    {mod, {boss_app, []}},
    {env, []},
    {applications, [kernel, stdlib, crypto]}]}.
