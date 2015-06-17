-module(tnesia_api).

-export([
	 write/2,
	 read/2,
	 remove/2,
	 query_fetch/1,
	 query_filtermap/2,
	 query_foreach/2,
	 query_raw/3
	]).

-include("tnesia.hrl").
-include("types.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% write
%%--------------------------------------------------------------------
-spec write(tnesia_timeline(), tnesia_record()) -> 
		   {tnesia_timeline(), tnesia_timepoint()}.
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
-spec read(tnesia_timeline(), tnesia_timepoint()) -> tnesia_record().
read(Timeline, Timepoint) ->
    tnesia_lib:read_timepoint(Timeline, Timepoint).

%%--------------------------------------------------------------------
%% remove
%%--------------------------------------------------------------------
-spec remove(tnesia_timeline(), tnesia_timepoint()) -> ok.
remove(Timeline, Timepoint) ->
    tnesia_lib:remove_timepoint(Timeline, Timepoint).

%%--------------------------------------------------------------------
%% query_fetch
%%--------------------------------------------------------------------
-spec query_fetch(tnesia_query()) -> [tnesia_record()].
query_fetch(Query) ->
    query_filtermap(
      Query, 
      fun(BaseRecord, _BagRecord, _Limit) -> {true, BaseRecord} end
     ).
%%--------------------------------------------------------------------
%% query_filtermap
%%--------------------------------------------------------------------
-spec query_filtermap(tnesia_query(), tnesia_filtermap_fun()) -> [tnesia_record()].
query_filtermap(Query, Fun) ->
    query_raw(Query, true, Fun).

%%--------------------------------------------------------------------
%% query_foreach
%%--------------------------------------------------------------------
-spec query_foreach(tnesia_query(), tnesia_foreach_fun()) -> ok.
query_foreach(Query, Fun) ->
    [] = query_raw(Query, false, Fun),
    ok.

%%--------------------------------------------------------------------
%% query_raw
%%--------------------------------------------------------------------
-spec query_raw(tnesia_query(), tnesia_return(), tnesia_fun()) -> [tnesia_record()].
query_raw(Query, Return, Fun) ->

    Timeline = proplists:get_value(timeline, Query, tnesia_lib:default_timeline()),
    Since = proplists:get_value(since, Query, tnesia_lib:default_since()),
    Till = proplists:get_value(till, Query, tnesia_lib:default_till()),
    Order = proplists:get_value(order, Query, tnesia_lib:default_order()),
    Limit = proplists:get_value(limit, Query, tnesia_lib:default_limit()),

    tnesia_lib:init_read_since_till(
      #tnesia_query{
	 bag = Timeline,
	 from = Since,
	 to = Till,
	 limit = Limit,
	 order = Order,
	 return = Return
	},
      Fun).
