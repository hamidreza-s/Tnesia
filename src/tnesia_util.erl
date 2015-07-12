-module(tnesia_util).

-compile(export_all).

-include("tnesia.hrl").

%%--------------------------------------------------------------------
%% type_of
%%--------------------------------------------------------------------
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_X)                     -> unknown.

%%--------------------------------------------------------------------
%% to_list
%%--------------------------------------------------------------------
to_list(X) when is_integer(X)   -> integer_to_list(X);
to_list(X) when is_float(X)     -> float_to_list(X);
to_list(X) when is_bitstring(X) -> bitstring_to_list(X);
to_list(X) when is_binary(X)    -> binary_to_list(X);
to_list(X) when is_atom(X)      -> atom_to_list(X);
to_list(X) when is_list(X)      -> X;
to_list(_X)                     -> "wrong type".

%%--------------------------------------------------------------------
%% to_binary
%%--------------------------------------------------------------------
to_binary(X) when is_integer(X)   -> integer_to_binary(X);
to_binary(X) when is_float(X)     -> float_to_binary(X);
to_binary(X) when is_atom(X)      -> atom_to_binary(X, unicode);
to_binary(X) when is_list(X)      -> list_to_binary(X);
to_binary(X) when is_bitstring(X) -> X;
to_binary(X) when is_binary(X)    -> X;
to_binary(_X)                     -> "wrong type".

%%--------------------------------------------------------------------
%% trim_string
%%--------------------------------------------------------------------
trim_string(right, Count, String) ->
    lists:sublist(String, 1, length(String) - Count);
trim_string(left, Count, String) ->
    lists:sublist(String, 1 + Count, length(String));
trim_string(both, Count, String) ->
    lists:sublist(String, 1 + Count, length(String) - Count).

%%--------------------------------------------------------------------
%% is_proplists
%%--------------------------------------------------------------------
is_proplists([]) -> true;
is_proplists([{_, _} | Tail]) -> is_proplists(Tail);
is_proplists(_) -> false.
    


