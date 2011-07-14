
.PHONY: onomatic test

REBAR=$(shell which rebar)

all: onomatic

onomatic :
	$(REBAR) compile
	$(REBAR) generate force=1

test :
	$(REBAR) compile eunit
