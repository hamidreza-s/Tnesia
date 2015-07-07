-module(tnesia_tql_common_SUITE).

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
%% tnesia_tql_selects
%%--------------------------------------------------------------------
tnesia_tql_select_1(_Config) ->

    Query = "select all from 'foo'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{all,1,"all"},
	{from,1,"from"},
	{atom_value,1,"foo"}]),
   
    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,
	[{timeline,{atom_value,1,"foo"}},
	 {keys,{all,1,"all"}}]}),

    ok.

tnesia_tql_select_2(_Config) ->

    Query = "select {'bar', 'bat'} from 'foo'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{list_values,1,["bar","bat"]},
	{from,1,"from"},
	{atom_value,1,"foo"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,
	[{timeline,{atom_value,1,"foo"}},
         {keys,{list_values,1,["bar", "bat"]}}]}),
       
    ok.

tnesia_tql_select_3(_Config) ->

    Query = "select {'bar'} from 'foo'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{list_values,1,["bar"]},
	{from,1,"from"},
	{atom_value,1,"foo"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,
	[{timeline,{atom_value,1,"foo"}},
         {keys,{list_values,1,["bar"]}}]}),
       
    ok.

tnesia_tql_select_4(_Config) ->

    Query = "select all from 'foo' where" ++ 
	" limit '100' and" ++ 
	" order 'des'",
    
    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{all,1,"all"},
	{from,1,"from"},
	{atom_value,1,"foo"},
	{where,1,"where"},
	{limit,1,"limit"},
	{atom_value,1,"100"},
	{conjunctive,1,"and"},
	{order,1,"order"},
	{atom_value,1,"des"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,[{timeline,{atom_value,1,"foo"}},
         {keys,{all,1,"all"}},
         {where,
	  [{limit,{atom_value,1,"100"}},
	   {order,{atom_value,1,"des"}}]}]}),
    
    ok.

tnesia_tql_select_5(_Config) ->

    Query = "select all from 'foo' where" ++ 
	" since '2010-01-01 12:00:00'" ++ 
	" till '2015-01-01 12:00:00:00'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{all,1,"all"},
	{from,1,"from"},
	{atom_value,1,"foo"},
	{where,1,"where"},
	{since,1,"since"},
	{atom_value,1,"2010-01-01 12:00:00"},
	{till,1,"till"},
	{atom_value,1,"2015-01-01 12:00:00:00"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,[{timeline,{atom_value,1,"foo"}},
		{keys,{all,1,"all"}},
		{where,
		 [{times,{{atom_value,1,"2010-01-01 12:00:00"},
			  {atom_value,1,"2015-01-01 12:00:00:00"}}}]}]}),
    ok.

  
tnesia_tql_select_6(_Config) ->

    Query = "select all from 'foo' where" ++ 
	" limit '100' and" ++ 
	" order 'des' and" ++ 
	" since '2010-01-01 12:00:00'" ++ 
	" till '2015-01-01 12:00:00:00'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{all,1,"all"},
	{from,1,"from"},
	{atom_value,1,"foo"},
	{where,1,"where"},
	{limit,1,"limit"},
	{atom_value,1,"100"},
	{conjunctive,1,"and"},
	{order,1,"order"},
	{atom_value,1,"des"},
	{conjunctive,1,"and"},
	{since,1,"since"},
	{atom_value,1,"2010-01-01 12:00:00"},
	{till,1,"till"},
	{atom_value,1,"2015-01-01 12:00:00:00"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,[{timeline,{atom_value,1,"foo"}},
		{keys,{all,1,"all"}},
		{where,
		 [{limit,{atom_value,1,"100"}},
		  {order,{atom_value,1,"des"}},
		  {times,{{atom_value,1,"2010-01-01 12:00:00"},
			  {atom_value,1,"2015-01-01 12:00:00:00"}}}]}]}),

    ok.

tnesia_tql_select_7(_Config) ->

    Query = "select all from 'foo' where" ++ 
	" 'foo' == 'bar' and" ++
	" limit '100' and" ++ 
	" order 'des' and" ++ 
	" since '2010-01-01 12:00:00'" ++ 
	" till '2015-01-01 12:00:00:00'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{all,1,"all"},
	{from,1,"from"},
	{atom_value,1,"foo"},
	{where,1,"where"},
	{atom_value,1,"foo"},
	{comparator,1,"=="},
	{atom_value,1,"bar"},
	{conjunctive,1,"and"},
	{limit,1,"limit"},
	{atom_value,1,"100"},
	{conjunctive,1,"and"},
	{order,1,"order"},
	{atom_value,1,"des"},
	{conjunctive,1,"and"},
	{since,1,"since"},
	{atom_value,1,"2010-01-01 12:00:00"},
	{till,1,"till"},
	{atom_value,1,"2015-01-01 12:00:00:00"}]),
    
    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,[{timeline,{atom_value,1,"foo"}},
		{keys,{all,1,"all"}},
		{where,
		 [{condition,{{atom_value,1,"foo"},
			      {comparator,1,"=="},
			      {atom_value,1,"bar"}}},
		  {limit,{atom_value,1,"100"}},
		  {order,{atom_value,1,"des"}},
		  {times,{{atom_value,1,"2010-01-01 12:00:00"},
			  {atom_value,1,"2015-01-01 12:00:00:00"}}}]}]}),
    ok.

tnesia_tql_select_8(_Config) ->

    Query = "select all from 'foo' where" ++ 
	" 'foo' == 'bar' and" ++
	" 'bal' < 'bat' and" ++
	" limit '100' and" ++ 
	" order 'des' and" ++ 
	" since '2010-01-01 12:00:00'" ++ 
	" till '2015-01-01 12:00:00:00'",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{select,1,"select"},
	{all,1,"all"},
	{from,1,"from"},
	{atom_value,1,"foo"},
	{where,1,"where"},
	{atom_value,1,"foo"},
	{comparator,1,"=="},
	{atom_value,1,"bar"},
	{conjunctive,1,"and"},
	{atom_value,1,"bal"},
	{comparator,1,"<"},
	{atom_value,1,"bat"},
	{conjunctive,1,"and"},
	{limit,1,"limit"},
	{atom_value,1,"100"},
	{conjunctive,1,"and"},
	{order,1,"order"},
	{atom_value,1,"des"},
	{conjunctive,1,"and"},
	{since,1,"since"},
	{atom_value,1,"2010-01-01 12:00:00"},
	{till,1,"till"},
	{atom_value,1,"2015-01-01 12:00:00:00"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {select,[{timeline,{atom_value,1,"foo"}},
		{keys,{all,1,"all"}},
		{where,
		 [{condition,{{atom_value,1,"foo"},
			      {comparator,1,"=="},
			      {atom_value,1,"bar"}}},
		  {condition,{{atom_value,1,"bal"},
			      {comparator,1,"<"},
			      {atom_value,1,"bat"}}},
		  {limit,{atom_value,1,"100"}},
		  {order,{atom_value,1,"des"}},
		  {times,{{atom_value,1,"2010-01-01 12:00:00"},
			  {atom_value,1,"2015-01-01 12:00:00:00"}}}]}]}),
       ok.

%%--------------------------------------------------------------------
%% tnesia_tql_inserts
%%--------------------------------------------------------------------
tnesia_tql_inserts_1(_Config) ->

    Query = "insert into 'foo' {'bar_key', 'bat_key'}" ++
	" records {'bar_val_1', 'bat_val_1'}",
    
    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{insert,1,"insert"},
	{into,1,"into"},
	{atom_value,1,"foo"},
	{list_values,1,["bar_key","bat_key"]},
	{records,1,"records"},
	{list_values,1,["bar_val_1","bat_val_1"]}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {insert,[{timeline,{atom_value,1,"foo"}},
		{keys,{list_values,1,["bar_key","bat_key"]}},
		{values,[{list_values,1,["bar_val_1","bat_val_1"]}]}]}),
    ok.

tnesia_tql_inserts_2(_Config) ->

    Query = "insert into 'foo' {'bar_key', 'bat_key'}" ++
	" records" ++ 
	" {'bar_val_1', 'bat_val_1'} and" ++
	" {'bar_val_2', 'bar_val_2'} and" ++
	" {'bar_val_3', 'bar_val_3'}",

    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{insert,1,"insert"},
	{into,1,"into"},
	{atom_value,1,"foo"},
	{list_values,1,["bar_key","bat_key"]},
	{records,1,"records"},
	{list_values,1,["bar_val_1","bat_val_1"]},
	{conjunctive,1,"and"},
	{list_values,1,["bar_val_2","bar_val_2"]},
	{conjunctive,1,"and"},
	{list_values,1,["bar_val_3","bar_val_3"]}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {insert,[{timeline,{atom_value,1,"foo"}},
		{keys,{list_values,1,["bar_key","bat_key"]}},
		{values,
		 [{list_values,1,["bar_val_1","bat_val_1"]},
		  {list_values,1,["bar_val_2","bar_val_2"]},
		  {list_values,1,["bar_val_3","bar_val_3"]}]}]}),
       
    ok.

%%--------------------------------------------------------------------
%% tnesia_tql_deletes
%%--------------------------------------------------------------------
tnesia_tql_deletes_1(_Config) ->

    Query = "delete from 'foo' when '123456789'",
    
    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    ?assertEqual(
       Tokens,
       [{delete,1,"delete"},
	{from,1,"from"},
	{atom_value,1,"foo"},
	{'when',1,"when"},
	{atom_value,1,"123456789"}]),

    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    ?assertEqual(
       AST,
       {delete,[{timeline,{atom_value,1,"foo"}},
		{record_time,{atom_value,1,"123456789"}}]}),

    ok.
