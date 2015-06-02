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

    T1 = tnesia_lib:get_micro_timestamp(now()),
    timer:sleep(1000),
    tnesia_api:write(Timeline, "test-record-1"),
    timer:sleep(1000),
    tnesia_api:write(Timeline, "test-record-2"),
    timer:sleep(1000),
    tnesia_api:write(Timeline, "test-record-3"),
    timer:sleep(1000),
    tnesia_api:write(Timeline, "test-record-4"),
    timer:sleep(1000),
    tnesia_api:write(Timeline, "test-record-5"),
    T2 = tnesia_lib:get_micro_timestamp(now()),

    Result = tnesia_api:query_filtermap(
	       [
		{timeline, Timeline},
		{since, T1},
		{till, T2},
		{order, des},
		{limit, 10}
	       ],
	       fun(BaseRecord, BagRecord, Limit) ->
		       {true, {BaseRecord, BagRecord, Limit}}
	       end
	      ),

    true = (length(Result) =:= 5),

    ok.
