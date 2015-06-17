-module(tnesia_common_bench_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("tnesia.hrl").

-record(tnesia_record, {id, value = [X || X <- lists:seq(1,32)]}).

-define(TIMELINE(Int), list_to_binary("timeline-" ++ integer_to_list(Int))).
-define(DEBUG(Format, Args), ct:print(default, 50, Format, Args)).

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
     {light_benchmark, [sequential], [get_ready, write_records, read_records]},
     {normal_benchmark, [sequential], [get_ready, write_records, read_records]},
     {heavy_benchmark, [sequential], [get_ready, write_records, read_records]}
    ].

all() ->
    [
     {group, light_benchmark},
     {group, normal_benchmark},
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
    TnesiaBenchConfig = {tnesia_bench_config, 
			 [
			  {read_concurrency, 1},
			  {read_total_records, 100},
			  {read_time_dispersion, 1},
			  {write_concurrency, 1},
			  {write_total_queries, 100}
			 ]},
    [TnesiaBenchConfig|Config];
init_per_group(normal_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, 
			 [
			  {read_concurrency, 5},
			  {read_total_records, 500},
			  {read_time_dispersion, 5},
			  {write_concurrency, 5},
			  {write_total_queries, 500}
			 ]},
    [TnesiaBenchConfig|Config];
init_per_group(heavy_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, 
			 [
			  {read_concurrency, 10},
			  {read_total_records, 1000},
			  {read_time_dispersion, 10},
			  {write_concurrency, 10},
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
    TnesiaBenchConfig = ?config(tnesia_bench_config, Config),

    WriteConcurrency = ?config(write_concurrency, TnesiaBenchConfig),
    WriteTotalQueries = ?config(write_total_queries, TnesiaBenchConfig),

    Self = self(),

    T1 = tnesia_lib:get_micro_timestamp(now()),

    lists:foreach(
      fun(ThreadNumber) ->
	      spawn(fun() ->
			    writer_loop(
			      Self,
			      ThreadNumber,
			      WriteTotalQueries
			     )
		    end)
      end,
      lists:seq(1, WriteConcurrency)
     ),

    T2 = tnesia_lib:get_micro_timestamp(now()),

    WriterResult = wait_for_result(WriteConcurrency),
    TimeResult = {time_result, [{start, T1}, {finish, T2}]},
    Result = [WriterResult, TimeResult],

    TnesiaWriteResult = {tnesia_write_result, Result},
    SavedConfig = raw_saved_config(Config),
    NewConfig = [TnesiaWriteResult|SavedConfig],

    {save_config, NewConfig}.

%%--------------------------------------------------------------------
%% read_records
%%--------------------------------------------------------------------
read_records(Config) ->
    TnesiaBenchConfig = ?config(tnesia_bench_config, Config),

    _ReadConcurrency = ?config(read_concurrency, TnesiaBenchConfig),
    _ReadTotalRecords = ?config(read_total_records, TnesiaBenchConfig),
    _ReadRecordSize = ?config(read_record_size, TnesiaBenchConfig),
    _ReadTimeDispersion = ?config(read_time_dispersion, TnesiaBenchConfig),

    TnesiaReadResult = {tnesia_read_result, [todo_read_result]},
    SavedConfig = raw_saved_config(Config),
    NewConfig = [TnesiaReadResult|SavedConfig],

    {save_config, NewConfig}.


%%====================================================================
%% Workers
%%====================================================================

%%--------------------------------------------------------------------
%% writer_loop
%%--------------------------------------------------------------------
writer_loop(CallerPID, ThreadNumber, WriteTotalQueries) 
  when WriteTotalQueries > 0 ->
    
    Timeline = ?TIMELINE(ThreadNumber),
    Record = #tnesia_record{id = WriteTotalQueries},

    tnesia_api:write(
      Timeline,
      Record
     ),
    
    writer_loop(CallerPID, ThreadNumber, WriteTotalQueries - 1);
writer_loop(CallerPID, ThreadNumber, _WriteTotalQueries) ->
    CallerPID ! {finish, {tread, ThreadNumber}}.

%%--------------------------------------------------------------------
%% reader_loop
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% wait_for_result
%%--------------------------------------------------------------------
wait_for_result(WriteConcurrency) ->
    wait_for_result(WriteConcurrency, []).

wait_for_result(WriteConcurrency, State) 
  when WriteConcurrency > 0 ->
    receive
	{finish, Result} ->
	    wait_for_result(WriteConcurrency - 1, [Result|State]);
	_ -> 
	    wait_for_result(WriteConcurrency, State)
    end;
wait_for_result(_WriteConcurrency, State) ->
    State.

%%====================================================================
%% Utilities
%%====================================================================

%%--------------------------------------------------------------------
%% print_report
%%--------------------------------------------------------------------
print_report(GroupName, Config) ->
    TnesiaBenchConfig = ?config(tnesia_bench_config, Config),
    TnesiaBenchResult = raw_saved_config(Config),
    TnesiaWriteResult = ?config(tnesia_write_result, TnesiaBenchResult),
    TnesiaReadResult = ?config(tnesia_read_result, TnesiaBenchResult),

    ct:print(
      default,
      50,
      "Benchmark: ~p~n" ++
	  "== ~nConfig:~n~p~n" ++
	  "== ~nWrite Result:~n~p~n" ++
	  "== ~nRead Result:~n~p~n",
      [GroupName, TnesiaBenchConfig, TnesiaWriteResult, TnesiaReadResult]
     ).

%%--------------------------------------------------------------------
%% raw_saved_config
%%--------------------------------------------------------------------
raw_saved_config(Config) ->
    case ?config(saved_config, Config) of
	{_SuiteName, SavedConfig} -> SavedConfig;
	_ -> []
    end.

