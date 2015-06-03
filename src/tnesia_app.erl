-module(tnesia_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(TNESIA_FRAGS, 32).

%%===================================================================
%% Application callbacks
%%===================================================================

start(_StartType, _StartArgs) ->
   application:ensure_started(mnesia),
   mnesia:change_table_copy_type(schema, node(), disc_copies),
   tnesia_lib:create_table(?TNESIA_FRAGS),
   tnesia_sup:start_link().

stop(_State) ->
   ok.
