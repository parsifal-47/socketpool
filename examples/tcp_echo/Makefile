REBAR?=./rebar3
SERVICE_NAME=tcp_echo
RELEASE_BINARY := _build/default/rel/$(SERVICE_NAME)/bin/$(SERVICE_NAME)

all: release

clean:
	$(REBAR) clean

distclean: clean
	git clean -fxd

release: compile
	$(REBAR) release

compile:
	$(REBAR) compile

start: release
	$(RELEASE_BINARY) start

stop: release
	$(RELEASE_BINARY) stop