REBAR := ./rebar

all: get-deps compile compile-app

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

compile-app:
	$(REBAR) boss c=compile

help:
	@echo 'Makefile for your chicagoboss app                                      '
	@echo '                                                                       '
	@echo 'Usage:                                                                 '
	@echo '   make help                        displays this help text            '
	@echo '   make get-deps                    updates all dependencies           '
	@echo '   make compile                     compiles dependencies              '
	@echo '   make compile-app                 compiles only your app             '
	@echo '                                    (so you can reload via init.sh)    '
	@echo '   make all                         get-deps compile compile-app       '
	@echo '                                                                       '
	@echo 'DEFAULT:                                                               '
	@echo '   make all                                                            '
	@echo '                                                                       '

.PHONY: all get-deps compile compile-app help
