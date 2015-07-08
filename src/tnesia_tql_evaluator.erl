-module(tnesia_tql_evaluator).

-export([
	 eval/1
	]).

-include("tnesia.hrl").
-include("types.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% eval
%%--------------------------------------------------------------------
eval({insert, AST}) ->
    {ok, insert(AST)};
eval({delete, AST}) ->
    {ok, delete(AST)};
eval({select, AST}) ->
    {ok, select(AST)};
eval(_) ->
    {nok, not_implemented}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% insert
%%--------------------------------------------------------------------
insert([{timeline, Timeline},
	{keys, Keys},
	{values, Values}]) ->
    pre_insert(Timeline, Keys, lists:reverse(Values), []).

pre_insert(
  Timeline, 
  Keys,
  [{list_values, _, Values} | RestValues],
  State) ->
    pre_insert(Timeline, Keys, RestValues, [Values | State]);
pre_insert(
  {atom_value, _, Timeline}, 
  {list_values, _, Keys},
  [], 
  Values) ->
    do_insert(Timeline, Keys, Values, []).

do_insert(Timeline, Keys, [Values | RestValues], Results) ->
    Result = ?API:write(
       Timeline,
       lists:zip(Keys, Values)),
    do_insert(Timeline, Keys, RestValues, [Result | Results]);
do_insert(_Timeline, _Keys, [], Results) ->
    {ok, lists:reverse(Results)}.

%%--------------------------------------------------------------------
%% delete
%%--------------------------------------------------------------------
delete([{timeline, {atom_value, _, Timeline}},
	{record_time, {atom_value, _, RecordTime}}]) ->
    ?API:remove(Timeline, list_to_integer(RecordTime)).

%%--------------------------------------------------------------------
%% select
%%--------------------------------------------------------------------
select([{timeline, Timeline},
	{keys, Keys}]) ->
    pre_select(Timeline, Keys, [], [], []);
select([{timeline, Timeline},
	{keys, Keys},
	{where, Wheres}]) ->
    pre_select(Timeline, Keys, Wheres, [], []).

pre_select(
  Timeline,
  Keys,
  [{times, {{atom_value, _, Since},
	    {atom_value, _, Till}}} | Wheres],
  TupleState, FunState) ->
    pre_select(
      Timeline, 
      Keys, 
      Wheres, 
      [{since, date_to_micro_timestamp(Since)}, 
       {till, date_to_micro_timestamp(Till)} | TupleState ],
      FunState);
pre_select(
 Timeline,
 Keys,
 [{limit, {atom_value, _, Limit}} | Wheres ],
 TupleState,
 FunState) ->
    pre_select(
      Timeline,
      Keys,
      Wheres,
      [{limit, list_to_integer(Limit)} | TupleState],
      FunState);
pre_select(
 Timeline,
 Keys,
 [{order, {direction, _, Order}} | Wheres ],
 TupleState,
 FunState) ->
    pre_select(
      Timeline,
      Keys,
      Wheres,
      [{order, list_to_atom(Order)} | TupleState],
      FunState);
pre_select(
 Timeline,
 Keys,
 [{condition, {{atom_value, _, Operand1}, 
	       {comparator, _, Comparator}, 
	       {atom_value, _, Operand2}}} | Wheres ],
 TupleState,
 FunState) ->
    pre_select(
      Timeline,
      Keys,
      Wheres,
      TupleState,
      [{Operand1, Comparator, Operand2} | FunState]);
pre_select(
  {atom_value, _, Timeline},
  {wildcard, _, _},
  [],
  TupleConditions,
  FunConditions) ->
    do_select(
      wildcard,
      [{timeline, Timeline} | TupleConditions],
      FunConditions
     );
pre_select(
 {atom_value, _, Timeline},
 {list_values, _, Keys},
 [],
 TupleConditions,
 FunConditions) ->
    do_select(
      Keys, 
      [{timeline, Timeline} | TupleConditions], 
      FunConditions).

do_select(Keys, TupleConditions, FunConditions) ->
    ?API:query_filtermap(
       TupleConditions,
       fun(Record, RecordIndex, RemainingLimit) ->
	       apply_fun_conditions_and_filter_keys(
		 {Record, RecordIndex, RemainingLimit},
		 FunConditions,
		 Keys)
       end).

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% apply_fun_conditions_and_filter_keys
%%--------------------------------------------------------------------
apply_fun_conditions_and_filter_keys(
  MetaRecord, 
  [FunCondition | FunConditions],
  Keys) ->

    {Record, _RecordIndex, _RemainingLimit} = MetaRecord,
    OrigVal = Record#tnesia_base.base_val,

    Bool =
	case FunCondition of
	    {WantedKey, "==", WantedVal} -> 
		TargetVal = ?LOOKUP(WantedKey, OrigVal),
		TargetVal =:= WantedVal;
	    
	    {WantedKey, "!=", WantedVal} -> 
		TargetVal = ?LOOKUP(WantedKey, OrigVal),
		TargetVal =/= WantedVal;

	    {WantedKey, ">", WantedVal} -> 
		TargetVal = ?LOOKUP(WantedKey, OrigVal),
		list_to_integer(TargetVal) > list_to_integer(WantedVal);

	    {WantedKey, ">=", WantedVal} ->
		TargetVal = ?LOOKUP(WantedKey, OrigVal),
		list_to_integer(TargetVal) >= list_to_integer(WantedVal);

	    {WantedKey, "<", WantedVal} ->
		TargetVal = ?LOOKUP(WantedKey, OrigVal),
		list_to_integer(TargetVal) < list_to_integer(WantedVal);

	    {WantedKey, "<=", WantedVal} ->
		TargetVal = ?LOOKUP(WantedKey, OrigVal),
		list_to_integer(TargetVal) =< list_to_integer(WantedVal)
	end,

    case Bool of
	true -> apply_fun_conditions_and_filter_keys(
		  MetaRecord, 
		  FunConditions,
		  Keys);
	false -> false
    end;
apply_fun_conditions_and_filter_keys(
  {Record, _RecordIndex, _RemainigLimit},
  [], 
  Keys) ->
    {Timeline, Timepoint} = Record#tnesia_base.base_key,
    RecordVal = Record#tnesia_base.base_val,

    FilteredVal = 
	case Keys of
	    wildcard -> RecordVal;
	    _ -> [{Key, ?LOOKUP(Key, RecordVal)} || Key <- Keys]
	end,

    {true, [{timeline, Timeline},
	    {timepoint, Timepoint},
	    {value, FilteredVal}]}.


%%--------------------------------------------------------------------
%% date_to_micro_timestamp
%%--------------------------------------------------------------------
date_to_micro_timestamp(Date) ->
    %% @TODO: get date and give micro second
    list_to_integer(Date).
    
  
