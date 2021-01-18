REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build-utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build-utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := woorl

# Build image to be used
BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 623eafbd7fb9be04ad54d878e00b85a99da8e88e

CALL_ANYWHERE    := all submodules compile xref lint dialyze test clean distclean format check_format
CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER)

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile:
	$(REBAR) escriptize

lint:
	elvis rock

check_format:
	$(REBAR) as test fmt -c

format:
	$(REBAR) fmt -w

xref:
	$(REBAR) xref

dialyze:
	$(REBAR) as test dialyzer

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build
