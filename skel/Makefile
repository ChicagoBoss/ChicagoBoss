ERL=erl
APP={{appid}}

all:
	$(ERL) -pa {{src}}/ebin -pa {{src}}/deps/*/ebin -eval 'boss_load:load_all_modules_and_emit_app_file($(APP), "ebin")' -s init stop -noshell

clean:
	rm -fv ebin/*.beam
	rm -fv ebin/$(APP).app
	
update_po:
	$(ERL) -pa ebin -pa {{src}}/ebin -eval -pa {{src}}/deps/*/ebin 'boss_load:load_models("ebin")' -eval 'boss_lang:update_po()' -s init stop -noshell

.PHONY: test
test:
	$(ERL) -pa $(PWD)/ebin -pa {{src}}/ebin -pa {{src}}/deps/*/ebin -run boss_web_test start $(APP) -noshell
