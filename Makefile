
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
ERL=erl
REBAR=./rebar
SESSION_CONFIG_DIR=priv/test_session_config

.PHONY: deps get-deps test

all:
	@$(REBAR) get-deps
	@$(REBAR) compile
	@echo ""
	@echo "*********************************************************************************"
	@echo ""
	@echo "CONGRATULATIONS! You've successfully built ChicagoBoss. Pat yourself on the back."
	@echo ""
	@echo "If you're unsure what to do next, try making a new app with:"
	@echo ""
	@echo "    make app PROJECT=my_project_name"
	@echo ""
	@echo "*********************************************************************************"
	@echo ""

boss:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean
	@rm -f src/boss/*.dtl.erl

edoc:
	$(ERL) -pa ebin -pa deps/*/ebin -run boss_doc run -noshell -s init stop
#$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop

app:
	@(if ! echo "$(PROJECT)" | grep -qE '^[a-z]+[a-zA-Z0-9_@]*$$'; then echo "Project name should be a valid Erlang atom."; exit 1; fi)
	@$(REBAR) create template=skel dest=$(DEST) appid=$(PROJECT) skip_deps=true
	@mkdir -p $(DEST)/deps
	@cp -Rn $(PWD) $(DEST)/deps/boss
	@mv -n $(DEST)/deps/boss/deps/* $(DEST)/deps/
	@echo ""
	@echo "***********************************************************************"
	@echo ""
	@echo "Your new app is created. You should head over there now:"
	@echo ""
	@echo "    cd $(DEST)"
	@echo ""
	@echo "***********************************************************************"
	@echo ""
	

get-deps:
	@$(REBAR) get-deps

deps:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit

test_session_cache:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/cache -noshell

test_session_mnesia:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mnesia -noshell

test_session_mock:
	$(ERL) -pa ebin -run boss_session_test start -config $(SESSION_CONFIG_DIR)/mock -noshell

rebarize:
	@mv $(APPDIR)/*.app.src $(APPDIR)/src
	@mkdir $(APPDIR)/priv/rebar
	@cp skel/priv/rebar/boss_plugin.erl $(APPDIR)/priv/rebar/boss_plugin.erl
	@cp skel/init.sh $(APPDIR)
	@chmod +x $(APPDIR)/init.sh
	@cp skel/init-dev.sh $(APPDIR)
	@chmod +x $(APPDIR)/init-dev.sh
	@cp skel/boss.config.* $(APPDIR)
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

