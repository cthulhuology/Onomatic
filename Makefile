
.PHONY: onomatic

REBAR=$(shell which rebar)

all: onomatic

onomatic :
	$(REBAR) compile
	$(REBAR) generate force=1
