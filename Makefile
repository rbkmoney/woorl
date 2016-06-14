REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)

.PHONY: all compile test clean distclean dialyze lint

all: compile

compile:
	$(REBAR) escriptize

rebar-update:
	$(REBAR) update

test:
	$(REBAR) eunit

xref:
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv .wercker _build _builds _cache _steps _temp

dialyze:
	$(REBAR) dialyzer

lint:
	elvis rock
