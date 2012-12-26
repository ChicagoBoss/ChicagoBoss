REBAR = $(shell pwd)/rebar
GZIP = $(shell pwd)/priv/util/gzip.sh
DEFLATE = $(shell pwd)/priv/util/deflate.sh

.PHONY: 

all: compile

compile:
	$(REBAR) boss c=compile

clean:
	$(REBAR) clean

start:
	./init-dev.sh


{{appid}}: all

gzip:
	$(GZIP)

deflate:
	$(DEFLATE)

###
### Docs
###
docs:
	@$(REBAR) skip_deps=true doc

