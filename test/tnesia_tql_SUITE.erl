-module(tnesia_tql_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("tnesia.hrl").

-define(SCANNER, tnesia_tql_scanner).
-define(PARSER, tnesia_tql_parser).
-define(DEBUG(Format, Args), ct:print(default, 50, Format, Args)).

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
%% tnesia_tql_scanner
%%--------------------------------------------------------------------
tnesia_tql_scanner(_Config) ->
    
    ?assertEqual({ok,[{select,1,"select"},
		      {all,1,"all"},
		      {from,1,"from"},
		      {string_value,1,"foo"}],
		  1},
		 ?SCANNER:string("select all from 'foo'")
		),

    ?assertEqual({ok,[{select,1,"select"},
		      {list_values,1,["bar","bat"]},
		      {from,1,"from"},
		      {string_value,1,"foo"}],
		  1}, 
		 ?SCANNER:string("select {'bar', 'bat'} from 'foo'")
		),
    
    ?assertEqual({ok,[{select,1,"select"},
		      {all,1,"all"},
		      {from,1,"from"},
		      {string_value,1,"foo"},
		      {where,1,"where"},
		      {order,1,"order"},
		      {asc,1,"asc"},
		      {conjunctive,1,"and"},
		      {limit,1,"limit"},
		      {integer_value,1,"10"}],
		  1},
		 ?SCANNER:string("select all from 'foo' where order asc and limit '10'")
		),
	
    ok.

%%--------------------------------------------------------------------
%% tnesia_tql_scanner
%%--------------------------------------------------------------------
tnesia_tql_parser(_Config) ->
    
    ok.
