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
    pre_insert(Timeline, Keys, Values, []).

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
    {ok, Results}.

%%--------------------------------------------------------------------
%% delete
%%--------------------------------------------------------------------
delete([{timeline, {atom_value, _, Timeline}},
	{record_time, {atom_value, _, RecordTime}}]) ->
    ?API:remove(Timeline, list_to_integer(RecordTime)).

%%--------------------------------------------------------------------
%% select
%%--------------------------------------------------------------------
select(AST) ->	 
    {ok, AST}.
