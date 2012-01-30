
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
ERL=erl
REBAR=./rebar
DB_CONFIG_DIR=src/boss/db_adapters/test_config
SESSION_CONFIG_DIR=src/boss/session_adapters/test_config

.PHONY: deps get-deps

all:
	@$(REBAR) compile skip_deps=true
	$(ERL) -pa ebin -pa deps/*/ebin \
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

get-deps:
	@$(REBAR) get-deps

deps:
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

rebarize:
	@mv $(APPDIR)/*.app.src $(APPDIR)/src
	@mkdir $(APPDIR)/priv/rebar
	@cp skel/priv/rebar/boss_plugin.erl $(APPDIR)/priv/rebar/boss_plugin.erl
	@cp skel/init.sh $(APPDIR)
	@chmod +x $(APPDIR)/init.sh
	@cp skel/init-dev.sh $(APPDIR)
	@chmod +x $(APPDIR)/init-dev.sh
	@cp skel/rebar $(APPDIR)
	@chmod +x $(APPDIR)/rebar
	@cp skel/rebar.config $(APPDIR)
	@mkdir $(APPDIR)/src/test/functional
	@find $(APPDIR)/src/test -maxdepth 1 -name "*.erl" -exec mv {} $(APPDIR)/src/test/functional \;
	@mkdir $(APPDIR)/src/test/eunit
	@echo $(APPDIR) rebar-boss-ified
	@echo WARNING: your boss.config have not been changed, you need to set:
	@echo - in boss app section:
	@echo ---- {path, \"$(PWD)\"}
	@echo ---- {vm_cookie, \"my_secret_cookie\"} % Optional, defaults to "abc123"
	@echo - for each app defined:
	@echo ---- {path, \"../path/to/app\"}
	@echo INFO: you can safely remove the Makefile and start* files from your app dir
	@echo INFO: after the boss.config change, you can run:
	@echo cd $(APPDIR)
	@echo ./rebar boss \# Shows all boss-rebar commands
	@echo ./init.sh    \# Shows the new boot system commands

