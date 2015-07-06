-module(tnesia_tql_api).

-export([
	 query/1
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
    {ok, Tokens, _} = ?TQL_SCANNER:string(Query),
    {ok, AST} = ?TQL_PARSER:parse(Tokens),
    {ok, ListResult} = ?TQL_EVALUATOR:eval(AST),
    ListResult.
    
%%====================================================================
%% Internal Functions
%%====================================================================

