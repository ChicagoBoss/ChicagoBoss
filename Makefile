
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
ERL=erl
REBAR=./rebar
DB_CONFIG_DIR=SRC/boss/db_adapters/test_config
SESSION_CONFIG_DIR=SRC/boss/session_adapters/test_config

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

edoc:
	$(ERL) -pa ebin -run boss_doc run -noshell -s init stop
#$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop

app:
	@$(REBAR) create template=skel dest=$(DEST) src=$(PWD) appid=$(PROJECT)

test_db_mock:
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/mock -noshell

test_db_mysql:
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/mysql -noshell

test_db_pgsql:
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/pgsql -noshell

test_db_mongodb:
	echo "db.boss_db_test_models.remove();"|mongo boss_test
	$(ERL) -pa ebin -run boss_db_test start -config $(DB_CONFIG_DIR)/mongodb -noshell

test_session_ets:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/ets -noshell

test_session_mnesia:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mnesia -noshell
