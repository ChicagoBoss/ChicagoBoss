ERL=erl
SERVER_APP=mochiweb.app
DRIVER_APP=medici.app
DB_APP=boss_db.app
TEMPLATE_APP=etcher.app
APP=boss.app
APPLICATION=boss
DEPS 	= erlydtl mochiweb etcher

all: ebin/$(APP) ebin/$(DB_APP) ebin/$(DRIVER_APP) ebin/$(SERVER_APP) ebin/$(TEMPLATE_APP) log
	$(ERL) -make

log:
	-mkdir -p log

ebin/$(APP): src/boss/$(APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DB_APP): src/boss/$(DB_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(DRIVER_APP): src/medici/$(DRIVER_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(SERVER_APP): src/mochiweb/$(SERVER_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(TEMPLATE_APP): src/etcher/$(TEMPLATE_APP)
	-mkdir -p ebin
	cp $< $@

clean:
	rm -fv ebin/*.beam
	-for a in $(DRIVER_APP) $(APP) $(SERVER_APP) $(DB_APP); do rm -fv ebin/$$a; done

edoc:
	-mkdir -p doc
	$(ERL) -pa ebin -noshell -eval "boss_doc:run()" -s init stop
