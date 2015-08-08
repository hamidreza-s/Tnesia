ERL := $(shell which erl)
CT := $(shell which ct_run)
HOST := $(shell hostname -s)
APP := tnesia
APP_NODE := $(APP)@$(HOST)
DEBUG_NODE := $(APP)_debug@$(HOST)
ATTACH_NODE := $(app)_attach@$(HOST)
COOKIE ?= secret
PWD := $(shell pwd)
REBAR := $(PWD)/rebar
LOGDIR := $(PWD)/logs
LOGFILE := $(LOGDIR)/$(APP).log
BENCHDIR := $(PWD)/bench
TESTDIR := $(PWD)/test


.PHONY: all compile deps clean test tql_test
	light_bench normal_bench heavy_bench start stop attach live

all: compile

compile: deps
	@exec $(REBAR) compile

deps:
	@exec $(REBAR) get-deps

clean:
	@rm -rf $(TESTDIR)/logs/*
	@rm -rf $(BENCHDIR)/logs/*
	@exec $(REBAR) clean

test: compile tql_test
	@mkdir -p $(TESTDIR)/logs
	@exec $(CT) \
		-dir $(TESTDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(TESTDIR)/logs

tql_test: compile
	@mkdir -p $(TESTDIR)/logs
	@exec $(CT) \
		-suite tnesia_tql_common_SUITE \
		-suite tnesia_tql_linter_SUITE \
		-suite tnesia_tql_api_SUITE \
		-suite tnesia_tql_formatter_SUITE \
		-dir $(TESTDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(TESTDIR)/logs

light_bench: compile
	@mkdir -p $(BENCHDIR)/logs
	@exec $(CT) -suite tnesia_common_bench_SUITE \
		-dir $(BENCHDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(BENCHDIR)/logs \
		-group light_benchmark

normal_bench: compile
	@mkdir -p $(BENCHDIR)/logs
	@exec $(CT) -suite tnesia_common_bench_SUITE \
		-dir $(BENCHDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(BENCHDIR)/logs \
		-group normal_benchmark

heavy_bench: compile
	@mkdir -p $(BENCHDIR)/logs
	@exec $(CT) -suite tnesia_common_bench_SUITE \
		-dir $(BENCHDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(BENCHDIR)/logs \
		-group heavy_benchmark

start:
	@mkdir -p $(LOGDIR)
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)/deps/*/ebin -s $(APP) start \
              -sname $(APP_NODE) -setcookie $(COOKIE) \
              -noshell -noinput > $(LOGFILE) &
	@echo "Tnesia was started!"

stop:
	@mkdir -p $(LOGDIR)
	@exec $(ERL) -sname $(DEBUG_NODE) -setcookie $(COOKIE) \
		-eval 'rpc:call($(APP_NODE),tnesia,stop,[]),init:stop()' \
		-noshell -noinput > $(LOGFILE) &
	@echo "Tnesia was stoped!"

attach:
	@exec $(ERL) -sname $(ATTACH_NODE) -setcookie $(COOKIE) \
		-remsh $(APP_NODE)

live: compile
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)/deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE)
