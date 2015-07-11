-module(tnesia_tql_linter_SUITE).

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
%% tnesia_tql_syntax_linter
%%--------------------------------------------------------------------
tnesia_tql_syntax_linter_1(_Config) ->

    Query = "select * from 'foo",
    Result = ?TQL_API:query_term(Query),
    ?assertEqual(
       Result,
       {error, "TQL Syntax Error: 'foo is illegal!"}),

    ok.

tnesia_tql_syntax_linter_2(_Config) ->

    Query = "select * from 'foo' XwhereX",
    Result = ?TQL_API:query_term(Query),
    ?assertEqual(
       Result,
       {error, "TQL Syntax Error: X is illegal!"}),

    ok.

%%--------------------------------------------------------------------
%% tnesia_tql_semantics_linter
%%--------------------------------------------------------------------
tnesia_tql_semantics_linter_1(_Config) ->

    Query = "select * from 'foo' where asc order",
    Result = ?TQL_API:query_term(Query),

    ?assertEqual(
       Result,
       {error, "TQL Semantics Error: there is somethig wrong around \"asc\"!"}),

    ok.

tnesia_tql_semantics_linter_2(_Config) ->

    Query = "select * from 'foo' where",
    Result = ?TQL_API:query_term(Query),

    ?assertEqual(
       Result,
       {error, "TQL Semantics Error: check your query!"}),

    ok.
