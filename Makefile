ERL=erl
ERLC=erlc
DB_TEST_APP=boss_db_test.app
SESSION_TEST_APP=boss_session_test.app
SERVER_APP=mochiweb.app
DRIVER_APP=medici.app
MONGODB_APP=mongodb.app
TRANSLATE_APP=boss_translator.app
APP=boss.app
APPLICATION=boss
DEPS 	= erlydtl mochiweb
PP_PARSER=SRC/aleppo/aleppo_parser
DB_CONFIG_DIR=SRC/boss/db_adapters/test_config
SESSION_CONFIG_DIR=SRC/boss/session_adapters/test_config

all: $(PP_PARSER).erl ebin/$(APP) ebin/$(DRIVER_APP) ebin/$(SERVER_APP) \
    ebin/$(TEMPLATE_APP) ebin/$(TRANSLATE_APP) ebin/$(DB_TEST_APP) ebin/$(SESSION_TEST_APP) ebin/$(MONGODB_APP) dirs
	$(ERL) -pa ebin -make

$(PP_PARSER).erl: $(PP_PARSER).yrl
	$(ERLC) -o SRC/aleppo $(PP_PARSER).yrl

dirs:
	-mkdir -p log lang lib

ebin/$(APP): SRC/boss/$(APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DRIVER_APP): SRC/medici/$(DRIVER_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(MONGODB_APP): SRC/mongodb/ebin/$(MONGODB_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(TRANSLATE_APP): SRC/boss/$(TRANSLATE_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DB_TEST_APP): SRC/boss/$(DB_TEST_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(SESSION_TEST_APP): SRC/boss/$(SESSION_TEST_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(SERVER_APP): SRC/mochiweb/$(SERVER_APP)
	-mkdir -p ebin
	cp $< $@DB

clean:
	rm -fv ebin/*.beam
	-for a in $(DRIVER_APP) $(MONGODB_APP) $(DB_TEST_APP) $(APP) $(SERVER_APP) $(TRANSLATE_APP); do rm -fv ebin/$$a; done

edoc:
	$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop

.PHONY: test
test:
	$(ERL) -pa ebin -run boss_web_test -noshell

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
		
