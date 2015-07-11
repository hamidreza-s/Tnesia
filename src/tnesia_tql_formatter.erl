-module(tnesia_tql_formatter).

-export([
	 term/1,
	 json/1
	]).

-export([
	 proplist_to_json_object/1,
	 list_to_json_array/1,
	 term_to_json_string/1
	]).

-include("tnesia.hrl").
-include("types.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% term
%%--------------------------------------------------------------------
term(Term) ->
    {ok, Term}.

%%--------------------------------------------------------------------
%% json
%%--------------------------------------------------------------------
json({select, Result}) ->
    {ok, select_format_json(Result)};
json({insert, Result}) ->
    {ok, insert_format_json(Result)};
json({delete, Result}) ->
    {ok, delete_format_json(Result)};
json({error, Result}) ->
    {ok, error_format_json(Result)}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% select_format_json
%%--------------------------------------------------------------------
select_format_json(Term) ->
    select_format_json(Term, []).
select_format_json([[{timeline, Timeline},
		     {timepoint, Timepoint},
		     {value, Value}] | Tail], State) ->
    Values = [{"__timeline__", Timeline},
	      {"__timepoint__", Timepoint} | Value],
    select_format_json(Tail, [proplist_to_json_object(Values) | State]);
select_format_json([], State) ->
    ?TO_BIN(list_to_json_array(State)).

%%--------------------------------------------------------------------
%% insert_format_json
%%--------------------------------------------------------------------
insert_format_json(Term) ->
    insert_format_json(Term, []).
insert_format_json([{_Timeline, Timepoint} | Tail], State) ->
    insert_format_json(Tail, [Timepoint | State]);
insert_format_json([], State) ->
    ?TO_BIN(list_to_json_array(State)).

%%--------------------------------------------------------------------
%% delete_format_json
%%--------------------------------------------------------------------
delete_format_json(Term) ->
    ?TO_BIN(term_to_json_string(Term)).

%%--------------------------------------------------------------------
%% error_format_json
%%--------------------------------------------------------------------
error_format_json(Term) ->
    ?TO_BIN(term_to_json_string(Term)).

%%--------------------------------------------------------------------
%% proplist_to_json_object
%%--------------------------------------------------------------------
proplist_to_json_object(Proplist) ->
    proplist_to_json_object(Proplist, "{").
proplist_to_json_object([{Key, Value} | Tail], State) ->
    proplist_to_json_object(
      Tail, 
      State ++ 
	  "\"" ++ ?TO_LIST(Key) ++ 
	  "\":\"" ++ ?TO_LIST(Value) ++ "\",");
proplist_to_json_object([], State) ->
    ?TRIM_STR(right, 1, State) ++ "}".

%%--------------------------------------------------------------------
%% list_to_json_array
%%--------------------------------------------------------------------
list_to_json_array(List) ->
    list_to_json_array(List, "[").
list_to_json_array([Head | Tail], State) ->
    list_to_json_array(Tail, State ++ ?TO_LIST(Head) ++ ",");
list_to_json_array([], State) ->
    ?TRIM_STR(right, 1, State) ++ "]".

%%--------------------------------------------------------------------
%% term_to_json_string
%%--------------------------------------------------------------------
term_to_json_string(Term) ->
    "\"" ++ ?TO_LIST(Term) ++ "\"".
