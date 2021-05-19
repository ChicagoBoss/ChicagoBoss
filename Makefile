ERL=erl
REBAR=./rebar3
SESSION_CONFIG_DIR=priv/test_session_config

.PHONY: get-deps test dialyze clean edoc

all: compile

compile:
	@$(REBAR) compile

edoc:
	$(ERL) -pa ebin -pa ./_build/default/lib/*/ebin -run boss_doc run -noshell -s init stop
#$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop

get-deps:
	@$(REBAR) get-deps

dialyze: all
	@$(REBAR) dialyzer

clean:
	@$(REBAR) clean --all
	rm -fv rebar.lock
	rm -fv rebar3.crashdump
	find doc/ -maxdepth 1 -type f -delete

test:
	@$(REBAR) do eunit -c, ct -c, proper -c, cover -v

test_session_cache:
	$(ERL) -pa _build/default/lib/boss/ebin/ -run boss_session_test start -config $(SESSION_CONFIG_DIR)/cache -noshell

test_session_mnesia:
	$(ERL) -pa _build/default/lib/boss/ebin/ -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mnesia -noshell

test_session_mock:
	$(ERL) -pa _build/default/lib/boss/ebin/ -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mock -noshell
