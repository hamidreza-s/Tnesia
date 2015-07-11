-module(tnesia_tql_api).

-export([
	 query/1,
	 query_term/1,
	 query_json/1,
	 query_format/2
	]).

-include("tnesia.hrl").
-include("types.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% query
%%--------------------------------------------------------------------
-spec query(string()) -> list().
query(Query) ->
    query_format(Query, term).
 
%%--------------------------------------------------------------------
%% query_format
%%--------------------------------------------------------------------
-spec query_term(string()) -> list().
query_term(Query) ->
    query_format(Query, term).

-spec query_json(string()) -> list().
query_json(Query) ->
    query_format(Query, json).

-spec query_format(string(), atom()) -> list().
query_format(Query, Format) ->
    {ok, Result} = 
    	?TQL_LINTER:check_and_run(
    	   Query,
    	   [{syntax, {?TQL_SCANNER, string}},
    	    {semantics, {?TQL_PARSER, parse}},
    	    {evaluate, {?TQL_EVALUATOR, eval}},
	    {format, {?TQL_FORMATTER, Format}}]),

    Result.
   
%%====================================================================
%% Internal Functions
%%====================================================================

