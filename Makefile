ERL=erl
ERLC=erlc
DB_APP=boss_db.app
APP=boss.app
APPLICATION=boss
DEPS 	= erlydtl mochiweb medici

all: ebin/$(APP) ebin/$(DB_APP)
	$(ERL) -make
#	-for d in $(DEPS); do (cd deps/$$d; $(MAKE) all) done

ebin/$(DB_APP): src/$(DB_APP)
	-mkdir -p ebin
	cp $< $@

ebin/$(APP): src/$(APP)
	-mkdir -p ebin
	cp $< $@

clean:
	rm -fv ebin/*.beam
	-for d in $(DEPS); do (cd deps/$$d; $(MAKE) clean) done
	rm -fv ebin/$(APP)

edoc:
	$(ERL) -noshell -eval "edoc:application($(APPLICATION), \".\", [])" \
	    -s init stop
