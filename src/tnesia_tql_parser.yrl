%%====================================================================
%% Nonterminals
%%====================================================================
Nonterminals
query
select_query
select_fields
select_wheres
select_where
select_where_time
select_where_order
select_where_limit
%% insert_query
%% delete_query
.

%%====================================================================
%% Terminals
%%====================================================================
Terminals
select
all
from
since
till
order
limit
where
asc
des
and
or
insert
records
delete
when
string_value
integer_value
list_values
comparator
conjunctive
.

%%====================================================================
%% Rootsymbol
%%====================================================================
Rootsymbol query.

%%====================================================================
%% Rules
%%====================================================================
query ->
    select_query :
    '$1'.

select_query ->
    select select_fields from string_value :
    {select, [{timeline, '$2'}, {from, '$4'}]}.

select_query ->
    select select_fields from string_value where select_wheres:
    {select, [{timeline, '$2'}, {from, '$4'}, {where, todo}]}.

select_fields ->
    all :
    '$1'.

select_fields ->
    list_values :
    '$1'.

select_wheres ->
   select_where :
   '$1'.

select_wheres ->
    select_where conjunctive select_where :
    {conjunctive, '$1', '$3'}.

select_wheres ->
   select_wheres conjunctive select_where :
   {conjunctive, '$1', '$3'}.

select_where ->
    select_where_time :
    ['$1'].

select_where ->
    select_where_order :
    ['$1'].

select_where ->
    select_where_limit :
    ['$1'].

select_where ->
    select_where_time select_where_order select_where_limit :
    ['$1', '$2', '$3'].

select_where_time ->
    since string_value till string_value :
    [{since, '$2'}, {till, '$4'}].

select_where_order ->
    order string_value :
    {order, '$2'}.

select_where_limit ->
    limit integer_value :
    {limit, '$2'}.

%%====================================================================
%% Erlang Code
%%====================================================================

