-module(tnesia_api_SUITE).
-include_lib("common_test/include/ct.hrl").

-include("tnesia.hrl").

-compile(export_all).

%%====================================================================
%% CT Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% suite | groups | all
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

%%--------------------------------------------------------------------
%% init_per_suite | end_per_suite
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(tnesia),
    Config.

end_per_suite(_Config) ->
    application:stop(tnesia),
    ok.

%%--------------------------------------------------------------------
%% init_per_group | end_per_group
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

end_per_group(_group, Config) ->
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
%% test_tnesia_api
%%--------------------------------------------------------------------
test_tnesia_api(_Config) ->

    Timeline = "test-timeline",
    RecordPrefix = "test-record-",
    RecordsCount = 10,
    MaxDelaySec = 1000,

    T1 = ?LIB:get_micro_timestamp(now()),
    
    lists:foreach(
      fun(Item) ->
	      timer:sleep(random:uniform(MaxDelaySec)),
	      ?API:write(
		Timeline, 
		RecordPrefix ++ integer_to_list(Item))
      end,
      lists:seq(1, RecordsCount)
     ),

    T2 = ?LIB:get_micro_timestamp(now()),
    
    QueryLimit = 20,
    QueryOrder = des,
    
    %% --- filtermapt test
    FiltermapResult = 
	?API:query_filtermap(
	  [
	   {timeline, Timeline},
	   {since, T1},
	   {till, T2},
	   {order, QueryOrder},
	   {limit, QueryLimit}
	  ],
	  fun(Record, RecordIndex, RemainingLimit) ->
		  {true, {Record, RecordIndex, RemainingLimit}}
	  end
	 ),
    
    if
	QueryLimit < RecordsCount ->
	    true = (length(FiltermapResult) =:= QueryLimit);
	true ->
	    true = (length(FiltermapResult) =:= RecordsCount)
    end,

    %% --- foreach test
    ForeachResult = 
	?API:query_foreach(
	  [
	   {timeline, Timeline},
	   {since, T1},
	   {till, T2},
	   {order, QueryOrder},
	   {limit, QueryLimit}
	  ],
	  fun(Record, RecordIndex, RemainingLimit) ->
		  {tnesia_base, {Timeline, _}, _} = Record,
		  {tnesia_bag, {Timeline, _}, {Timeline, _}} = RecordIndex,
		  true = (RemainingLimit =< QueryLimit),
		  
		  true
	  end
	 ),
    
    ok = ForeachResult,

    ok.
