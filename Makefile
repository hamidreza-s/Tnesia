ERL := $(shell which erl)
CT := $(shell which ct_run)
APP := tnesia
COOKIE ?= secret
PWD := $(shell pwd)
REBAR := $(PWD)/rebar
LOGDIR := $(PWD)/logs
LOGFILE := $(LOGDIR)/$(APP).log
BENCHDIR := $(PWD)/bench
TESTDIR := $(PWD)/test


.PHONY: all compile deps clean test bench start live

all: compile

compile: deps
	@exec $(REBAR) compile

deps:
	@exec $(REBAR) get-deps

clean:
	@rm -rf $(TESTDIR)/logs/*
	@rm -rf $(BENCHDIR)/logs/*
	@exec $(REBAR) clean

test: compile
	@exec $(CT) \
		-dir $(TESTDIR) \
		-include $(PWD)/include \
		-pa $(PWD)/ebin \
		-logdir $(TESTDIR)/logs

tql_test: compile
	@exec $(CT) -suite tnesia_tql_SUITE \
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

start: compile
	@mkdir -p $(LOGDIR)
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE) \
              -noshell -noinput > $(LOGFILE) &

live: compile
	@exec $(ERL) -pa $(PWD)/ebin $(PWD)/deps/*/ebin -s $(APP) \
              -sname $(APP) -setcookie $(COOKIE)
