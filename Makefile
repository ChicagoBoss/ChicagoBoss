ERL=erl
DRIVER_APP=medici.app
DB_APP=boss_db.app
APP=boss.app
APPLICATION=boss
DEPS 	= erlydtl mochiweb

all: ebin/$(APP) ebin/$(DB_APP) ebin/$(DRIVER_APP) log
	$(ERL) -make
#	-for d in $(DEPS); do (cd deps/$$d; $(MAKE) all) done

log:
	-mkdir -p log

ebin/$(APP): src/$(APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DB_APP): src/$(DB_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DRIVER_APP): src/medici/$(DRIVER_APP)
	-mkdir -p ebin
	cp $< $@

clean:
	rm -fv ebin/*.beam
	-for d in $(DEPS); do (cd deps/$$d; $(MAKE) clean) done
	rm -fv ebin/$(APP)

edoc:
	-mkdir -p doc
	cp src/overview.edoc doc/
	$(ERL) -noshell -eval "edoc:application($(APPLICATION), \".\", [{subpackages, false}])" \
	    -s init stop

site: site/boss_doc_templates
	$(ERL) -pa ebin -pa deps/*/ebin -noshell -eval "boss_doc:run()" -s init stop
