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
	Error -> next({error, Error}, Actions)
    end;
next({tokens, Tokens}, Actions) ->
    {Mod, Fun} = ?LOOKUP(semantics, Actions),
    case Mod:Fun(Tokens) of
	{ok, AST} -> next({ast, AST}, Actions);
	Error -> next({error, Error}, Actions)
    end;
next({ast, AST}, Actions) ->
    {Mod, Fun} = ?LOOKUP(evaluate, Actions),
    case Mod:Fun(AST) of
	{ok, List} -> next({list, List}, Actions);
	Error -> next({error, Error}, Actions)
    end;
next({list, List}, Actions) ->
    {Mod, Fun} = ?LOOKUP(format, Actions),
    case Mod:Fun(List) of
	{ok, Formated} -> next({formated, Formated}, Actions);
	Error -> next({error, Error}, Actions)
    end;
next({error, List}, Actions) ->
    {ok, Reason} = check_error(List),
    {Mod, Fun} = ?LOOKUP(format, Actions),
    {ok, Formated} = Mod:Fun({error, Reason}),
    next({formated, Formated}, Actions);
next({formated, Formated}, _Actions) ->
    {ok, Formated}.

%%--------------------------------------------------------------------
%% check_error
%%--------------------------------------------------------------------
check_error({error, {_Line1, _Module, {illegal, Token}}, _Line2}) ->
    Reason = make_syntax_error("~s is illegal!", [Token]),
    {ok, Reason};
check_error({error, {_Line1, _Module, [_ErrorDesc, []]}}) ->
    Reason = make_semantics_error("check your query!", []),
    {ok, Reason};
check_error({error, {_Line1, _Module, [_ErrorDesc, ErrorToken]}}) ->
    Reason = make_semantics_error(
	       "there is somethig wrong around ~s!", [ErrorToken]),
    {ok, Reason};
check_error(_Error) -> 
    Reason = make_semantics_error("there is something wrong!", []),
    {ok, Reason}.

%%--------------------------------------------------------------------
%% make_syntax_error
%%--------------------------------------------------------------------
make_syntax_error(Form, Args) ->
    ?FORMAT("TQL Syntax Error: " ++ Form, Args).

%%--------------------------------------------------------------------
%% make_semantics_error
%%--------------------------------------------------------------------
make_semantics_error(Form, Args) ->
    ?FORMAT("TQL Semantics Error: " ++ Form, Args).
