%%====================================================================
%% Definitions
%%====================================================================
Definitions.

WhiteSpace = ([\s])
ControlChars = ([\000-\037])
Comparator = (==|!=|>|>=|<|<=)
StringValues = [A-Za-z]
IntegerValues = [0-9]
ListValues = \{(.*)\}

%%====================================================================
%% Rules
%%====================================================================
Rules.

select : {token, {select, TokenLine, TokenChars}}.
all : {token, {all, TokenLine, TokenChars}}.
from : {token, {from, TokenLine, TokenChars}}.
since : {token, {since, TokenLine, TokenChars}}.
till : {token, {till, TokenLine, TokenChars}}.
order : {token, {order, TokenLine, TokenChars}}.
limit : {token, {limit, TokenLine, TokenChars}}.
where : {token, {where, TokenLine, TokenChars}}.

insert : {token, {insert, TokenLine, TokenChars}}.
records : {token, {records, TokenLine, TokenChars}}.

delete : {token, {delete, TokenLine, TokenChars}}.
when : {token, {'when', TokenLine, TokenChars}}.

and : {token, {conjunctive, TokenLine, TokenChars}}.
or : {token, {conjunctive, TokenLine, TokenChars}}.

des : {token, {des, TokenLine, TokenChars}}.
asc : {token, {asc, TokenLine, TokenChars}}.

'{StringValues}+' : {token, {string_value, TokenLine, strip_val(TokenChars, TokenLen)}}.
'{IntegerValues}+' : {token, {integer_value, TokenLine, strip_val(TokenChars, TokenLen)}}.

{ListValues}+ : {token, {list_values, TokenLine, parse_list(TokenChars)}}.

{Comparator}+ : {token, {comparator, TokenLine, TokenChars}}.
{WhiteSpace}+ : skip_token.
{ControlChars}+ : skip_token.

%%====================================================================
%% Erlang Code
%%====================================================================
Erlang code.

strip_val(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).

parse_list(TokenChars) ->
    L1 = string:strip(TokenChars, left, ${),
    L2 = string:strip(L1, right, $}),
    L3 = string:tokens(L2, ","),
    L4 = [string:strip(Item, both, $\s) || Item <- L3],
    L5 = [strip_val(Item, length(Item)) || Item <- L4],
    L5.