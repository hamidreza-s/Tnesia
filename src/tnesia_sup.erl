-module(tnesia_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%===================================================================
%% API functions
%%===================================================================

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init([]) ->
   {ok, { {one_for_one, 5, 10}, []} }.

