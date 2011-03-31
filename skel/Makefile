ERL=erl

all:
	$(ERL) -pa {{src}}/ebin -eval 'boss_load:load_all_modules("ebin")' -s init stop -noshell

clean:
	rm -fv ebin/*.beam

.PHONY: test
test:
	$(ERL) -pa {{src}}/ebin -run boss_web_test -noshell
