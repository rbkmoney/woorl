REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build-utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build-utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := woorl

# Build image tag to be used
BUILD_IMAGE_TAG   := 562313697353c29d4b34fb081a8b70e8c2207134
SERVICE_IMAGE_TAG := 562313697353c29d4b34fb081a8b70e8c2207134

CALL_ANYWHERE    := all submodules compile xref lint dialyze test clean distclean
CALL_W_CONTAINER := $(CALL_ANYWHERE)
.PHONY: $(CALL_W_CONTAINER)

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

all: compile

compile:
	$(REBAR) escriptize

lint:
	elvis rock

xref:
	$(REBAR) xref

dialyze:
	$(REBAR) dialyzer

test:
	$(REBAR) eunit

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build
