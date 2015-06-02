ERL := $(shell which erl)
APP := tnesia
COOKIE ?= secret
PWD := $(shell pwd)
REBAR := $(PWD)/rebar
LOGDIR := $(PWD)logs
LOGFILE := $(LOGDIR)/$(APP).log

.PHONY: all compile deps test start live

all: compile

compile: deps
	@exec $(REBAR) compile

deps:
	@exec $(REBAR) get-deps

test:
	@exec $(REBAR) ct

start:
	@mkdir -p $(LOGDIR)
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE) \
              -noshell -noinput > $(LOGFILE) &

live:
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)/deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE)
