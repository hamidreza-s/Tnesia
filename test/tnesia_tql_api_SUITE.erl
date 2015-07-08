-module(tnesia_tql_api_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

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
%% tql_api_tests
%%--------------------------------------------------------------------
tql_api_test_1(_Config) ->
    
    Timeline = timeline(),
    Key1 = key1(), Key2 = key2(), Key3 = key3(),
    Keys = "{'" ++ Key1 ++ "', '" ++ Key2 ++ "', '" ++ Key3 ++ "'}",
    Record1 = record1(), Record2 = record2(), Record3 = record3(),

    %% --- insert query
    InsertQuery = "insert into '" ++ Timeline ++ "' " ++ Keys ++
	" records " ++ Record1 ++ 
	" and " ++ Record2 ++
	" and " ++ Record3,

    {ok, [{Timeline, 
	   _Record1Timepoint}, 
	  {Timeline, 
	   _Record2Timepoint},
	  {Timeline,
	   _Record3Timepoint}]} = ?TQL_API:query(InsertQuery),

    %% --- select query
    SelectQuery =
	"select " ++ Keys ++
	" from '" ++ Timeline ++ "'",
    
    SelectResult = ?TQL_API:query(SelectQuery),
    ?assertEqual(length(SelectResult), 3),

    ok.

tql_api_test_2(_Config) ->
    
    Timeline = timeline(),
    Key1 = key1(), Key2 = key2(), Key3 = key3(),
    Keys = "{'" ++ Key1 ++ "', '" ++ Key2 ++ "', '" ++ Key3 ++ "'}",
    Record1 = record1(), Record2 = record2(), Record3 = record3(),

    %% --- insert query
    InsertQuery = "insert into '" ++ Timeline ++ "' " ++ Keys ++
	" records " ++ Record1 ++ 
	" and " ++ Record2 ++
	" and " ++ Record3,

    {ok, [{Timeline, 
	   Record1Timepoint}, 
	  {Timeline, 
	   _Record2Timepoint},
	  {Timeline,
	   Record3Timepoint}]} = ?TQL_API:query(InsertQuery),

    %% --- select query
    SelectQuery = "select * from '" ++ Timeline ++ "' where" ++
	" since '" ++ integer_to_list(Record1Timepoint) ++ "'" ++
	" till '" ++ integer_to_list(Record3Timepoint) ++ "'",
    
    SelectResult = ?TQL_API:query(SelectQuery),
    ?assertEqual(length(SelectResult), 3),

    ok.

tql_api_test_3(_Config) ->

    Timeline = timeline(),
    Key1 = key1(), Key2 = key2(), Key3 = key3(),
    Keys = "{'" ++ Key1 ++ "', '" ++ Key2 ++ "', '" ++ Key3 ++ "'}",
    Record1 = record1(), Record2 = record2(), Record3 = record3(),
    
    %% --- insert query
    InsertQuery = "insert into '" ++ Timeline ++ "' " ++ Keys ++
	" records " ++ Record1 ++ 
	" and " ++ Record2 ++
	" and " ++ Record3,

    {ok, [{Timeline, 
	   _Record1Timepoint}, 
	  {Timeline, 
	   _Record2Timepoint},
	  {Timeline,
	   _Record3Timepoint}]} = ?TQL_API:query(InsertQuery),

    %% --- select query
    SelectQuery =
	"select {'" ++ Key1 ++ "', '" ++ Key2 ++ "'}" ++
	" from '" ++ Timeline ++ "' where" ++
	" limit '1'",
    
    [SelectResult] =  ?TQL_API:query(SelectQuery),
    ?assertMatch(
       [{Key1, _}, {Key2, _}],
       ?LOOKUP(value, SelectResult)),

    ok.

tql_api_test_4(_Config) ->

    Timeline = timeline(),
    Key1 = key1(), Key2 = key2(), Key3 = key3(),
    Keys = "{'" ++ Key1 ++ "', '" ++ Key2 ++ "', '" ++ Key3 ++ "'}",
    Record1 = record1(), Record2 = record2(), Record3 = record3(),
    
    %% --- insert query
    InsertQuery = "insert into '" ++ Timeline ++ "' " ++ Keys ++
	" records " ++ Record1 ++ 
	" and " ++ Record2 ++
	" and " ++ Record3,

    {ok, [{Timeline, 
	   Record1Timepoint}, 
	  {Timeline, 
	   Record2Timepoint},
	  {Timeline,
	   Record3Timepoint}]} = ?TQL_API:query(InsertQuery),

    %% --- select query
    SelectQuery =
	"select " ++ Keys ++
	" from '" ++ Timeline ++ "' where" ++
	" since '" ++ integer_to_list(Record1Timepoint) ++ "'" ++
	" till '" ++ integer_to_list(Record3Timepoint) ++ "'" ++
	" and order des and limit '100'",

    SelectResult1 =  ?TQL_API:query(SelectQuery),
    ?assertEqual(length(SelectResult1), 3),

    %% --- delete query
    DeleteRecord1Query = "delete from '" ++ Timeline ++ 
	"' when '" ++ integer_to_list(Record1Timepoint) ++ "'",

    DeleteRecord2Query = "delete from '" ++ Timeline ++
	"' when '" ++ integer_to_list(Record2Timepoint) ++ "'",

    ok = ?TQL_API:query(DeleteRecord1Query),
    ok = ?TQL_API:query(DeleteRecord2Query),

    SelectResult2 = ?TQL_API:query(SelectQuery),
    ?assertEqual(length(SelectResult2), 1),

    ok.

tql_api_test_5(_Config) ->

    Timeline = timeline(),
    Key1 = key1(), Key2 = key2(), Key3 = key3(),
    Keys = "{'" ++ Key1 ++ "', '" ++ Key2 ++ "', '" ++ Key3 ++ "'}",
    Record1 = record1(), Record2 = record2(), Record3 = record3(),

    %% --- insert query
    InsertQuery = "insert into '" ++ Timeline ++ "' " ++ Keys ++
	" records " ++ Record1 ++ 
	" and " ++ Record2 ++
	" and " ++ Record3,

    {ok, [{Timeline, 
	   Record1Timepoint}, 
	  {Timeline, 
	   _Record2Timepoint},
	  {Timeline,
	   Record3Timepoint}]} = ?TQL_API:query(InsertQuery),

    %% --- select query
    SelectQuery =
	"select " ++ Keys ++
	" from '" ++ Timeline ++ "' where" ++
	" since '" ++ integer_to_list(Record1Timepoint) ++ "'" ++
	" till '" ++ integer_to_list(Record3Timepoint) ++ "'" ++
	" and order des and limit '100'" ++
	" and '" ++ Key2 ++ "' == 'null'" ++
	" and '" ++ Key3 ++ "' < '3'",

    SelectResult1 =  ?TQL_API:query(SelectQuery),
    ?assertEqual(length(SelectResult1), 1),

    %% --- delete query
    DeleteRecord3Query = "delete from '" ++ Timeline ++
	"' when '" ++ integer_to_list(Record3Timepoint) ++ "'",
    
    ok = ?TQL_API:query(DeleteRecord3Query),

    SelectResult2 = ?TQL_API:query(SelectQuery),
    ?assertEqual(length(SelectResult2), 0),

    ok.

%%====================================================================
%% Test data
%%====================================================================

%%--------------------------------------------------------------------
%% timeline
%%--------------------------------------------------------------------
timeline() -> "tweets".

%%--------------------------------------------------------------------
%% keys
%%--------------------------------------------------------------------
key1() -> "text".
key2() -> "media".
key3() -> "favourites".

%%--------------------------------------------------------------------
%% records
%%--------------------------------------------------------------------
record1() -> "{'Hello Tnesia', 'null', '4'}".
record2() -> "{'Tnesia Logo', 'tnesia.png', '100'}".
record3() -> "{'Bye Tnesia', 'null', '0'}".
