{application, boss_translator,
    [{description, "BossTranslator I18n engine"},
    {vsn, "0.01"},
    {modules, [
        boss_translator,
        boss_translator_app,
        boss_translator_controller,
        boss_translator_sup
            ]},
    {registered, []},
    {mod, {boss_translator_app, []}},
    {env, []},
    {applications, [kernel, stdlib]}]}.
