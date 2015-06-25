-module(tnesia_common_bench_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("tnesia.hrl").

-record(tnesia_record, {id, value}).

-define(VALUE(ByteSize), [X || X <- lists:seq(1, ByteSize)]).
-define(TIMELINE(Int), list_to_binary("timeline-" ++ integer_to_list(Int))).
-define(DEBUG(Format, Args), ct:print(default, 50, Format, Args)).

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
			  {read_total_queries, 1000},
			  {read_count_limit, 50},
			  {read_time_length, {10, second}},
			  {write_concurrency, 1},
			  {write_total_queries, 10000},
			  {write_record_bytesize, 32}
			 ]},
    [TnesiaBenchConfig|Config];
init_per_group(normal_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, 
			 [
			  {read_concurrency, 2},
			  {read_total_queries, 1000},
			  {read_count_limit, 50},
			  {read_time_length, {10, second}},
			  {write_concurrency, 2},
			  {write_total_queries, 10000},
			  {write_record_bytesize, 32}
			 ]},
    [TnesiaBenchConfig|Config];
init_per_group(heavy_benchmark, Config) ->
    TnesiaBenchConfig = {tnesia_bench_config, 
			 [
			  {read_concurrency, 4},
			  {read_total_queries, 1000},
			  {read_count_limit, 50},
			  {read_time_length, {10, second}},
			  {write_concurrency, 4},
			  {write_total_queries, 10000},
			  {write_record_bytesize, 32}
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
    WriteRecordByteSize = ?config(write_record_bytesize, TnesiaBenchConfig),
    WriteTotalQueries = ?config(write_total_queries, TnesiaBenchConfig),

    WriteTotalQueriesPerThread = WriteTotalQueries / WriteConcurrency,

    QueryInfo = [{write_record_bytesize, WriteRecordByteSize}],

    Self = self(),

    T1 = tnesia_lib:get_micro_timestamp(now()),

    lists:foreach(
      fun(ThreadNumber) ->
	      spawn(fun() ->
			    writer_loop(
			      Self,
			      ThreadNumber,
			      WriteTotalQueriesPerThread,
			      QueryInfo
			     )
		    end)
      end,
      lists:seq(1, WriteConcurrency)
     ),


    ThreadsResult = wait_for_result(WriteConcurrency),

    T2 = tnesia_lib:get_micro_timestamp(now()),

    TimeDiff = micro_to_second(T2 - T1),

    WriterResult = {write_result, ThreadsResult},
    TimeResult = {time_result, [
				{start, T1}, 
				{finish, T2},
				{diff, TimeDiff}
			       ]},
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

    ReadConcurrency = ?config(read_concurrency, TnesiaBenchConfig),
    ReadTotalQueries = ?config(read_total_queries, TnesiaBenchConfig),

    ReadTotalQueriesPerThread = ReadTotalQueries / ReadConcurrency,

    ReadCountLimit = ?config(read_count_limit, TnesiaBenchConfig),
    ReadTimeLength = ?config(read_time_length, TnesiaBenchConfig),

    RawSavedConfig = raw_saved_config(Config),
    TnesiaWriteResult = proplists:get_value(tnesia_write_result, RawSavedConfig),
    TnesiaWriteTimes = proplists:get_value(time_result, TnesiaWriteResult),
    TnesiaWriteStart = proplists:get_value(start, TnesiaWriteTimes),
    TnesiaWriteFinish = proplists:get_value(finish, TnesiaWriteTimes),

    %% log("times:~n~p", [{s, TnesiaWriteStart, f, TnesiaWriteFinish}]),

    QueryInfo = [
		 {time_start, TnesiaWriteStart},
		 {time_finish, TnesiaWriteFinish},
		 {read_total_queries, ReadTotalQueriesPerThread},
		 {read_count_limit, ReadCountLimit},
		 {read_time_length, ReadTimeLength}
		],

    Self = self(),

    T1 = tnesia_lib:get_micro_timestamp(now()),

    lists:foreach(
      fun(ThreadNumber) ->
	      spawn(fun() ->
			    reader_loop(
			      Self,
			      ThreadNumber,
			      ReadTotalQueriesPerThread,
			      QueryInfo
			     )
		    end)
      end,
      lists:seq(1, ReadConcurrency)
     ),

    ThreadsResult = wait_for_result(ReadConcurrency),

    T2 = tnesia_lib:get_micro_timestamp(now()),

    TimeDiff = micro_to_second(T2 - T1),

    ReaderResult = {read_result, ThreadsResult},
    TimeResult = {time_result, [
				{start, T1}, 
				{finish, T2}, 
				{diff, TimeDiff}
				]},
    Result = [ReaderResult, TimeResult],

    TnesiaReadResult = {tnesia_read_result, Result},
    SavedConfig = raw_saved_config(Config),
    NewConfig = [TnesiaReadResult|SavedConfig],

    {save_config, NewConfig}.

%%====================================================================
%% Workers
%%====================================================================

%%--------------------------------------------------------------------
%% writer_loop
%%--------------------------------------------------------------------
writer_loop(CallerPID, ThreadNumber, WriteTotalQueries, QueryInfo) 
  when WriteTotalQueries > 0 ->
    
    Timeline = ?TIMELINE(ThreadNumber),

    RecordByteSize = proplists:get_value(write_record_bytesize, QueryInfo),

    Record = #tnesia_record{
		id = WriteTotalQueries,
		value = ?VALUE(RecordByteSize)
	       },

    tnesia_api:write(
      Timeline,
      Record
     ),
    
    writer_loop(CallerPID, ThreadNumber, WriteTotalQueries - 1, QueryInfo);
writer_loop(CallerPID, ThreadNumber, _WriteTotalQueries, _QueryInfo) ->
    CallerPID ! {finish, {tread, ThreadNumber}}.

%%--------------------------------------------------------------------
%% reader_loop
%%--------------------------------------------------------------------
reader_loop(CallerPID, ThreadNumber, RemainingReadQueries, QueryInfo) 
  when RemainingReadQueries > 0 ->
    
    Timeline = ?TIMELINE(ThreadNumber),

    TimeStart = proplists:get_value(time_start, QueryInfo),
    TimeFinish = proplists:get_value(time_finish, QueryInfo),
    ReadTimeLength = proplists:get_value(read_time_length, QueryInfo),

    ReadTimeLengthValue = get_micro_second(ReadTimeLength),

    X = TimeFinish - TimeStart,
    TimeSince = random:uniform(X) + TimeStart,
    TimeTill = TimeSince + ReadTimeLengthValue,

    %% log("start - end: ~p - ~p~nsince - till: ~p - ~p", 
    %% [TimeStart, TimeFinish, TimeSince, TimeTill]),
    
    _QueryResult = tnesia_api:query_fetch(
      [
       {timeline, Timeline},
       {since, TimeSince},
       {till, TimeTill},
       {order, asc},
       {limit, unlimited}
      ]
     ),
    
    %% log("read query result:~n~p", [QueryResult]),

    reader_loop(CallerPID, ThreadNumber, RemainingReadQueries - 1, QueryInfo);
reader_loop(CallerPID, ThreadNumber, _RemainingReadQueries, _QueryInfo) ->
    CallerPID ! {finish, {thread, ThreadNumber}}.

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
%% log
%%--------------------------------------------------------------------
log(Format, Arguments) ->
    ct:print(default, 50, Format, Arguments).

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
	  "--------------------------- ~nConfig:~n~p~n" ++
	  "--------------------------- ~nWrite Result:~n~p~n" ++
	  "--------------------------- ~nRead Result:~n~p~n",
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

%%--------------------------------------------------------------------
%% get_micro_second
%%--------------------------------------------------------------------
get_micro_second({Int, second}) ->
    Int * 1000000;
get_micro_second({Int, minute}) ->
    Int * 60 * 1000000.

%%--------------------------------------------------------------------
%% micro_to_second
%%--------------------------------------------------------------------
micro_to_second(Micro) ->
    {Micro / 1000000, second}.
