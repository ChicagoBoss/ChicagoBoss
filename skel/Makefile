ERL=erl

all:
	$(ERL) -pa {{src}}/ebin -eval 'boss_load:load_all_modules_and_emit_app_file("ebin", {{appid}})' -s init stop -noshell

clean:
	rm -fv ebin/*.beam
	rm -fv ebin/{{appid}}.app
	
update_po:
	$(ERL) -pa ebin -pa ../ChicagoBoss/ebin -eval 'boss_load:load_models("ebin")' -eval 'boss_lang:update_po()' -s init stop -noshell

.PHONY: test
test:
	$(ERL) -pa {{src}}/ebin -run boss_web_test -noshell
