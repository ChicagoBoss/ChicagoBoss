
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
ERL=erl
REBAR=./rebar
DB_CONFIG_DIR=src/boss/db_adapters/test_config
SESSION_CONFIG_DIR=src/boss/session_adapters/test_config

all:
	@$(REBAR) compile skip_deps=true
	$(ERL) -pa ebin \
		-eval 'erlydtl:compile("src/boss/boss_html_error_template.dtl", boss_html_error_template, [{out_dir, "ebin"}])' \
		-eval 'erlydtl:compile("src/boss/boss_html_doc_template.dtl", boss_html_doc_template, [{out_dir, "ebin"}])' \
		-noshell -s init stop

clean:
	@$(REBAR) clean

edoc:
	$(ERL) -pa ebin -run boss_doc run -noshell -s init stop
#$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop

app:
	@$(REBAR) create template=skel dest=$(DEST) src=$(PWD) appid=$(PROJECT) skip_deps=true

deps:
	@$(REBAR) get-deps

mongodb:
	$(ERL) -make

riak:
	@$(REBAR) compile

test_db_mock:
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/mock -noshell

test_db_mysql:
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/mysql -noshell

test_db_pgsql:
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/pgsql -noshell

test_db_mongodb:
	echo "db.boss_db_test_models.remove();"|mongo boss_test
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/mongodb -noshell

test_session_cache:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/cache -noshell

test_session_mnesia:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mnesia -noshell

test_session_mock:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mock -noshell

test_db_riak:
	$(ERL) -pa ebin -pa deps/*/ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/riak -noshell
