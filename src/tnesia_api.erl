-module(tnesia_api).

-export([
	 write/2,
	 read/2,
	 remove/2
	]).

-include("tnesia.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% write
%%--------------------------------------------------------------------
write(Timeline, Record) ->
    Timepoint = tnesia_lib:write(
	      #tnesia_input{
		 timeline = Timeline,
		 timepoint = now(),
		 record = Record}),
    {Timeline, Timepoint}.

%%--------------------------------------------------------------------
%% read
%%--------------------------------------------------------------------
read(Timeline, Timepoint) ->
    tnesia_lib:read_timepoint(Timeline, Timepoint).

%%--------------------------------------------------------------------
%% remove
%%--------------------------------------------------------------------
remove(Timeline, Timepoint) ->
    tnesia_lib:remove_timepoint(Timeline, Timepoint).
