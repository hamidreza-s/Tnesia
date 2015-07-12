-module(tnesia_tql_formatter_SUITE).

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
    Config.

end_per_suite(_Config) ->
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
%% tnesia_tql_formatter_json
%%--------------------------------------------------------------------
tnesia_tql_formatter_json_1(_Config) ->
    
    ?assertEqual(
       "{\"foo\":\"bar\",\"bat\":\"123\"}",
       tnesia_tql_formatter:proplist_to_json_object(
	 [{foo, "bar"}, {<<"bat">>, 123}])),
    
    ok.

tnesia_tql_formatter_json_2(_Config) ->
    
    ?assertEqual(
       "[foo,bar,bat,123]",
       tnesia_tql_formatter:list_to_json_array(
	 ["foo", <<"bar">>, bat, 123])),
    
    ok.

tnesia_tql_formatter_json_3(_Config) ->
    
    ?assertEqual(
       "\"foo\"",
       tnesia_tql_formatter:term_to_json_string(foo)),
    
    ?assertEqual(
       "\"bar\"",
       tnesia_tql_formatter:term_to_json_string("bar")),
    
    ?assertEqual(
       "\"bat\"",
       tnesia_tql_formatter:term_to_json_string(<<"bat">>)),
    
    ?assertEqual(
       "\"123\"",
       tnesia_tql_formatter:term_to_json_string(123)),
    
    ok.
