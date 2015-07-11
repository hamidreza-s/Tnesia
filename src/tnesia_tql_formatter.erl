-module(tnesia_tql_formatter).

-export([
	 term/1,
	 json/1
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
    select_format_json(Term, "["). 
select_format_json([[{timeline, _Timeline},
		     {timepoint, _Timepoint},
		     {value, Value}] | Tail], State) ->
    select_format_json(Tail, State  ++ proplist_to_json(Value) ++ ",");
select_format_json([], State) ->
    Json = lists:sublist(State, 1, length(State) - 1) ++ "]",
    list_to_binary(Json).

%%--------------------------------------------------------------------
%% insert_format_json
%%--------------------------------------------------------------------
insert_format_json(Term) ->
    insert_format_json(Term, "[").
insert_format_json([{_Timeline, Timepoint} | Tail], State) ->
    insert_format_json(Tail, State ++ integer_to_list(Timepoint) ++ ",");
insert_format_json([], State) ->
    Json = lists:sublist(State, 1, length(State) - 1) ++ "]",
    list_to_binary(Json).

%%--------------------------------------------------------------------
%% delete_format_json
%%--------------------------------------------------------------------
delete_format_json(Term) ->
    Json = "\"" ++ atom_to_list(Term) ++ "\"",
    list_to_binary(Json).

%%--------------------------------------------------------------------
%% error_format_json
%%--------------------------------------------------------------------
error_format_json(Term) ->
    list_to_binary("\"" ++ Term ++ "\"").

%%--------------------------------------------------------------------
%% proplist_to_json
%%--------------------------------------------------------------------
proplist_to_json(Proplist) ->
    proplist_to_json(Proplist, "{").
proplist_to_json([{Key, Value} | Tail], State) ->
    proplist_to_json(Tail, State ++ "\"" ++ Key ++ "\":\"" ++ Value ++ "\",");
proplist_to_json([], State) ->
    lists:sublist(State, 1, length(State) - 1) ++ "}".
