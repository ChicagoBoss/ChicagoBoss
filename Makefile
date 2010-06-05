ERL=erl
SERVER_APP=mochiweb.app
DRIVER_APP=medici.app
TRANSLATE_APP=boss_translator.app
APP=boss.app
APPLICATION=boss
DEPS 	= erlydtl mochiweb

all: ebin/$(APP) ebin/$(DRIVER_APP) ebin/$(SERVER_APP) ebin/$(TEMPLATE_APP) ebin/$(TRANSLATE_APP) dirs
	$(ERL) -make

dirs:
	-mkdir -p log Lang

ebin/$(APP): src/boss/$(APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DRIVER_APP): src/medici/$(DRIVER_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(TRANSLATE_APP): src/boss/$(TRANSLATE_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(SERVER_APP): src/mochiweb/$(SERVER_APP)
	-mkdir -p ebin
	cp $< $@

clean:
	rm -fv ebin/*.beam
	-for a in $(DRIVER_APP) $(APP) $(SERVER_APP); do rm -fv ebin/$$a; done

edoc:
	-mkdir -p doc
	$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop

test: 
	erl -pa ebin -run boss_test -noshell
