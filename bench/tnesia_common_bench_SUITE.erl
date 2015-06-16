-module(tnesia_common_bench_SUITE).
-inclue_lib("common_test/include/ct.hrl").

-include("tnesia.hrl").

-compile(export_all).

%%====================================================================
%% Benchmark Scenario
%%
%% - read
%%  - input: concurrency, total_records, record_size, time_dispersion
%%  - output: total_time, per_read_time, resource_usage
%%
%% - write
%%  - input: concurrency, query_params, total_queries
%%  - output: total_time, per_write_time, resource_usage
%%====================================================================

%%====================================================================
%% CT Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% suite | groups | all
%%--------------------------------------------------------------------
suite() -> [].

groups() -> 
    [
     {light_benchmark, [sequential], [get_ready, read_records, write_records]},
     {normal_benchmark, [sequential], [get_ready, read_records, write_records]},
     {heavy_benchmark, [sequential], [get_ready, read_records, write_records]}
    ].

all() ->
    [
     %{group, light_benchmark},
     %{group, normal_benchmark},
     {group, heavy_benchmark}
    ].


%%--------------------------------------------------------------------
%% init_per_suite | end_per_suite
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% init_per_group | end_per_group
%%--------------------------------------------------------------------
init_per_group(light_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, [
		    {read_concurrency, 1},
		    {read_total_records, 100},
		    {read_record_size, 10},
		    {read_time_dispersion, 1},
		    {write_concurrency, 1},
		    {write_query_params, todo},
		    {write_total_queries, 100}
		   ]},
    [TnesiaBenchConfig|Config];
init_per_group(normal_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, [
		    {read_concurrency, 5},
		    {read_total_records, 500},
		    {read_record_size, 50},
		    {read_time_dispersion, 5},
		    {write_concurrency, 5},
		    {write_query_params, todo},
		    {write_total_queries, 500}
		   ]},
    [TnesiaBenchConfig|Config];
init_per_group(heavy_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, [
		    {read_concurrency, 10},
		    {read_total_records, 1000},
		    {read_record_size, 100},
		    {read_time_dispersion, 10},
		    {write_concurrency, 10},
		    {write_query_params, todo},
		    {write_total_queries, 1000}
		   ]},
    [TnesiaBenchConfig|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(GroupName, Config) ->
    print_report(GroupName, Config),
    Config.

%%--------------------------------------------------------------------
%% init_per_testcase | end_per_testcase
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% get_ready
%%--------------------------------------------------------------------
get_ready(_Config) ->
    tnesia_lib:delete_table(),
    application:stop(tnesia),
    application:start(tnesia),
    ok.
%%--------------------------------------------------------------------
%% write_records
%%--------------------------------------------------------------------
write_records(Config) ->
    TnesiaBenchConfig = proplists:get_value(tnesia_bench_config, Config),

    _WriteConcurrency = proplists:get_value(write_concurrency, TnesiaBenchConfig),
    _WriteQueryParams = proplists:get_value(write_query_params, TnesiaBenchConfig),
    _WriteTotalQueries = proplists:get_value(write_total_queries, TnesiaBenchConfig),

    TnesiaWriteResult = {tnesia_write_result, []},
    {save_config, [TnesiaWriteResult|Config]}.

%%--------------------------------------------------------------------
%% read_records
%%--------------------------------------------------------------------
read_records(Config) ->
    TnesiaBenchConfig = proplists:get_value(tnesia_bench_config, Config),

    _ReadConcurrency = proplists:get_value(read_concurrency, TnesiaBenchConfig),
    _ReadTotalRecords = proplists:get_value(read_total_records, TnesiaBenchConfig),
    _ReadRecordSize = proplists:get_value(read_record_size, TnesiaBenchConfig),
    _ReadTimeDispersion = proplists:get_value(read_time_dispersion, TnesiaBenchConfig),

    TnesiaReadResult = {tnesia_read_result, []},
    {save_config, [TnesiaReadResult|Config]}.

%%====================================================================
%% Utils
%%====================================================================

%%--------------------------------------------------------------------
%% print_report
%%--------------------------------------------------------------------
print_report(GroupName, Config) ->
    TnesiaBenchConfig = proplists:get_value(tnesia_bench_config, Config),
    TnesiaWriteResult = [todo],
    TnesiaReadResult = [todo],

    ct:print(
      default,
      50,
      "== ~nBenchmark: ~p~n" ++
	  "== ~nConfig:~n~p~n" ++
	  "== ~nWrite Result:~n~p~n" ++
	  "== ~nRead Result:~n~p~n" ++
          "== ~n",
      [GroupName, TnesiaBenchConfig, TnesiaWriteResult, TnesiaReadResult]
     ).

