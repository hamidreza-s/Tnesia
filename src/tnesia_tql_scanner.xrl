%%====================================================================
%% Definitions
%%====================================================================
Definitions.

WhiteSpace = ([\s])
ControlChars = ([\000-\037])
Comparator = (==|!=|>|>=|<|<=)
StringValues = [A-Za-z]
IntegerValues = [0-9]

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

'{StringValues}+' : {token, {string_value, TokenLine, strip_val(TokenChars, TokenLen)}}.
'{IntegerValues}+' : {token, {integer_value, TokenLine, strip_val(TokenChars, TokenLen)}}.

{Comparator}+ : {token, {comparator, TokenLine, TokenChars}}.
{WhiteSpace}+ : skip_token.
{ControlChars}+ : skip_token.

%%====================================================================
%% Erlang Code
%%====================================================================
Erlang code.

strip_val(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
