ERL=erl
REBAR=./rebar

all:
	$(ERL) -pa {{src}}/ebin -run 'boss_load:load_all_modules("ebin")' -s init stop

clean:
	rm -fv ebin/*.beam

.PHONY: test
test:
	$(ERL) -pa {{src}}/ebin -run boss_web_test -noshell
