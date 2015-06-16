ERL := $(shell which erl)
CT := $(shell which ct_run)
APP := tnesia
COOKIE ?= secret
PWD := $(shell pwd)
REBAR := $(PWD)/rebar
LOGDIR := $(PWD)logs
LOGFILE := $(LOGDIR)/$(APP).log
BENCHDIR := $(PWD)/bench

.PHONY: all compile deps test bench start live

all: compile

compile: deps
	@exec $(REBAR) compile

deps:
	@exec $(REBAR) get-deps

test: compile
	@exec $(REBAR) ct

bench: compile
	@mkdir -p $(BENCHDIR)/logs
	@exec $(CT) -suite tnesia_common_bench_SUITE \
		-dir $(BENCHDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(BENCHDIR)/logs

start: compile
	@mkdir -p $(LOGDIR)
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE) \
              -noshell -noinput > $(LOGFILE) &

live: compile
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)/deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE)
