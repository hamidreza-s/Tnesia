-module(tnesia_tql_linter).

-export([
	 check_and_run/2
	]).

-include("tnesia.hrl").
-include("types.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% check_and_run
%%--------------------------------------------------------------------
check_and_run(String, Actions) ->
    start(String, Actions).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% start
%%--------------------------------------------------------------------
start(String, Actions) ->
    next({string, String}, Actions).

%%--------------------------------------------------------------------
%% next
%%--------------------------------------------------------------------
next({string, String}, Actions) ->
    {Mod, Fun} = ?LOOKUP(syntax, Actions),
    case Mod:Fun(String) of
	{ok, Tokens, _} -> next({tokens, Tokens}, Actions);
	Error -> stop(Error)
    end;
next({tokens, Tokens}, Actions) ->
    {Mod, Fun} = ?LOOKUP(semantics, Actions),
    case Mod:Fun(Tokens) of
	{ok, AST} -> next({ast, AST}, Actions);
	Error -> stop(Error)
    end;
next({ast, AST}, Actions) ->
    {Mod, Fun} = ?LOOKUP(evaluate, Actions),
    case Mod:Fun(AST) of
	{ok, List} -> next({list, List}, Actions);
	Error -> stop(Error)
    end;
next({list, List}, Actions) ->
    {Mod, Fun} = ?LOOKUP(format, Actions),
    case Mod:Fun(List) of
	{ok, JSON} -> next({json, JSON}, Actions);
	Error -> stop(Error)
    end;
next({json, JSON}, _Actions) ->
    {ok, JSON}.

%%--------------------------------------------------------------------
%% stop
%%--------------------------------------------------------------------
stop({error, {_Line1, _Module, {illegal, Token}}, _Line2}) ->
    Reason = format_syntax_error("~s is illegal!", [Token]),
    {ok, Reason};
stop({error, {_Line1, _Module, [_ErrorDesc, []]}}) ->
    Reason = format_semantics_error("check your query!", []),
    {ok, Reason};
stop({error, {_Line1, _Module, [_ErrorDesc, ErrorToken]}}) ->
    Reason = format_semantics_error(
	       "there is somethig wrong around ~s!", [ErrorToken]),
    {ok, Reason};
stop(Error) -> 
    {ok, Error}.

%%--------------------------------------------------------------------
%% format_syntax_error
%%--------------------------------------------------------------------
format_syntax_error(Form, Args) ->
    ?FORMAT("TQL Syntax Error: " ++ Form, Args).

%%--------------------------------------------------------------------
%% format_semantics_error
%%--------------------------------------------------------------------
format_semantics_error(Form, Args) ->
    ?FORMAT("TQL Semantics Error: " ++ Form, Args).
