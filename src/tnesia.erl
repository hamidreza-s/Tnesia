-module(tnesia).

-export([create_table/1,
         delete_table/0,
         recreate_table/1]).

-export([table_info/0,
         run_test/0]).

-export([write/1,
         read/1,
         read_count/1,
         read_count_if/2,
         read_count_do/2,
         read_since/1,
         read_since_if/2,
         read_since_do/2,
         read_since_till/1,
         read_since_till_if/2,
         read_since_till_do/2]).

-include("tnesia.hrl").

-define(TIME_COUNT_LIMIT_SEC, 3600 * 24 * 30).
-define(TIME_STEP_PERCISION_SEC, 60).

-ifdef(debug).
-define(TNESIA_LOG(Msg, Args), 
   io:format(
      "[debug] - "
      ++ 
      Msg, 
      Args
   )
).
-else.
-define(TNESIA_LOG(_Msg, _Args), dont).
-endif.

%%====================================================================
%% Table Management API
%%====================================================================

%%--------------------------------------------------------------------
%% create_table
%%--------------------------------------------------------------------
create_table(Fragments) ->

   mnesia:create_table(tnesia_base, [
      {disc_copies, [node()]},
      {attributes, record_info(fields, tnesia_base)},
      {type, set},
      {frag_properties, [
         {node_pool, [node()]},
         {n_fragments, Fragments},
         {n_disc_copies, 1}
      ]}
   ]),

   mnesia:create_table(tnesia_bag, [
      {disc_copies, [node()]},
      {attributes, record_info(fields, tnesia_bag)},
      {type, bag},
      {frag_properties, [
         {node_pool, [node()]},
         {n_fragments, Fragments},
         {n_disc_copies, 1}
      ]}
   ]),

   ok.

%%--------------------------------------------------------------------
%% delete_table
%%--------------------------------------------------------------------
delete_table() ->
   
   [mnesia:delete_table(Table) || Table
      <- [tnesia_base, tnesia_bag]],

   ok.

%%--------------------------------------------------------------------
%% recreate_table
%%--------------------------------------------------------------------
recreate_table(Fragments) ->
   ok = delete_table(),
   ok = create_table(Fragments),
   ok.

%%--------------------------------------------------------------------
%% seed_sample_data
%%--------------------------------------------------------------------
seed_sample_data(Counter, Round) when Counter > 0 ->
   CounterBin = integer_to_binary(Counter),
   lists:foreach(
      fun(R) ->
         RBin = integer_to_binary(R),
         write(#tnesia_input{
            id = <<"id-", CounterBin/binary>>, 
            timestamp = now(), 
            record = #tnesia_sample{
               foo = <<RBin/binary, "-foo-", CounterBin/binary>>,
               bar = <<RBin/binary, "-bar-", CounterBin/binary>>,
               bat = <<RBin/binary, "-bat-", CounterBin/binary>>
            }
         })
      end,
      lists:seq(1, Round)
   ),
   seed_sample_data(Counter - 1, Round);
seed_sample_data(_, _) -> ok.

%%--------------------------------------------------------------------
%% table_info
%%--------------------------------------------------------------------
table_info() ->
   BaseSize = query_on_frags(
      async_dirty,
      fun() -> mnesia:table_info(tnesia_base, size) end
   ),

   BagSize = query_on_frags(
      async_dirty,
      fun() -> mnesia:table_info(tnesia_bag, size) end
   ),

   [{tnesia_base, BaseSize}, {tnesia_bag, BagSize}].

%%====================================================================
%% Query API
%%====================================================================

%%--------------------------------------------------------------------
%% write
%%--------------------------------------------------------------------
write(#tnesia_input{id = ID, timestamp = TimeStamp, record = Record} = _Input) ->

   MicroTimeStamp = get_micro_timestamp(TimeStamp),

   BaseRecord = #tnesia_base{
      base_key = {ID, MicroTimeStamp},
      base_val = Record
   },

   TimeStep = get_micro_timestep(MicroTimeStamp),

   BagRecord = #tnesia_bag{
      bag_key = {ID, TimeStep},
      base_key = BaseRecord#tnesia_base.base_key
   },

   WriteFun = fun() ->
      mnesia:write(BaseRecord),
      mnesia:write(BagRecord)
   end,

   mnesia:activity(transaction, WriteFun, [], mnesia_frag).

%%--------------------------------------------------------------------
%% read
%%--------------------------------------------------------------------
read(ID) ->
   query_on_frags(
      async_dirty, 
      fun() ->
         mnesia:read({tnesia_base, ID})
      end
   ).

%%--------------------------------------------------------------------
%% read_count
%%--------------------------------------------------------------------
read_count(Query) ->
   read_count_if(Query, fun(_Val, _Limit) -> true end).

%%--------------------------------------------------------------------
%% read_count_if
%%--------------------------------------------------------------------
read_count_if(Query, Fun) ->
   Now = now(),
   TimeStampFrom = get_micro_timestamp_count_limit(Now),
   TimeStampTo = get_micro_timestamp(Now),
   read_since_till_if(
      Query#tnesia_query{
         from = TimeStampFrom, 
         to = TimeStampTo,
         order = des
      },
      Fun
   ).
  
%%--------------------------------------------------------------------
%% read_count_do
%%--------------------------------------------------------------------
read_count_do(Query, Fun) ->
   Now = now(),
   TimeStampFrom = get_micro_timestamp_count_limit(Now),
   TimeStampTo = get_micro_timestamp(Now),
   read_since_till_do(
      Query#tnesia_query{
         from = TimeStampFrom, 
         to = TimeStampTo,
         order = des
      },
      Fun
   ).

%%--------------------------------------------------------------------
%% read_since
%%--------------------------------------------------------------------
read_since(Query) ->
   read_since_if(Query, fun(_Val, _Limit) -> true end).

%%--------------------------------------------------------------------
%% read_since_if
%%--------------------------------------------------------------------
read_since_if(Query, Fun) ->
   TimeStampTo = get_micro_timestamp(now()),
   read_since_till_if(Query#tnesia_query{to = TimeStampTo}, Fun).

%%--------------------------------------------------------------------
%% read_since_do
%%--------------------------------------------------------------------
read_since_do(Query, Fun) ->
   TimeStampTo = get_micro_timestamp(now()),
   read_since_till_do(Query#tnesia_query{to = TimeStampTo}, Fun).

%%--------------------------------------------------------------------
%% read_since_till
%%--------------------------------------------------------------------
read_since_till(Query) ->
   read_since_till_if(Query, fun(_Val, _Limit) -> true end).

%%--------------------------------------------------------------------
%% read_since_till_if
%%--------------------------------------------------------------------
read_since_till_if(Query, Fun) ->
   init_read_since_till(Query#tnesia_query{return = true}, Fun).

%%--------------------------------------------------------------------
%% read_since_till_do
%%--------------------------------------------------------------------
read_since_till_do(Query, Fun) ->
   init_read_since_till(Query#tnesia_query{return = false}, Fun).

%%====================================================================
%% Tools
%%====================================================================

%%--------------------------------------------------------------------
%% init_read_since_till_do
%%--------------------------------------------------------------------
init_read_since_till(#tnesia_query{
      from = TimeStampFrom,
      to = TimeStampTo,
      order = Order
   } = Query, 
   Fun
   ) ->

   ?TNESIA_LOG(
      "from: ~p - to: ~p~n", 
      [TimeStampFrom, TimeStampTo]
   ),

   TimeStepFrom = get_micro_timestep(TimeStampFrom),
   TimeStepTo = get_micro_timestep(TimeStampTo),
   TimeStep =
      case Order of
         asc -> TimeStepFrom;
         des -> TimeStepTo
      end,

   State = [],
   run_tnesia_query(
      Query,
      TimeStampFrom,
      TimeStampTo,
      TimeStep, 
      TimeStepFrom, 
      TimeStepTo, 
      Fun, 
      State
   ).

%%--------------------------------------------------------------------
%% run_tnesia_query
%%--------------------------------------------------------------------
run_tnesia_query(#tnesia_query{
      bag = Bag,
      limit = Limit,
      order = Order,
      return = Return
   } = Query, 
   TimeStampFrom,
   TimeStampTo,
   TimeStep, 
   TimeStepFrom, 
   TimeStepTo, 
   Fun,
   State
   ) when 
      TimeStepFrom =< TimeStepTo, Limit > 0;
      TimeStepFrom =< TimeStepTo, Limit =:= unlimited 
         ->
 
   ?TNESIA_LOG(
      "timestep: ~p~n", 
      [TimeStep]
   ),
  
   BagRecords = query_on_frags(
      async_dirty,
      fun() ->
         mnesia:read({tnesia_bag, {Bag, TimeStep}})
      end
   ),
  
   SortedBagRecords =
      case Order of
         asc -> BagRecords;
         des -> lists:reverse(BagRecords)
      end,

   ?TNESIA_LOG("return: ~p~n", [Return]),

   %% @TODO: check it!
   {NewLimit, NewState} =
      case Return of
         false -> cast_on_bag(SortedBagRecords, Query, Fun, State);
         true -> call_on_bag(SortedBagRecords, Query, Fun, State)
      end,

   {NewTimeStampFrom, NewTimeStampTo} =
      case Order of
         asc -> {TimeStampFrom + get_micro_timestep_precision(), TimeStampTo};
         des -> {TimeStampFrom, TimeStampTo - get_micro_timestep_precision()}
      end,

   NewTimeStepFrom = get_micro_timestep(NewTimeStampFrom),
   NewTimeStepTo = get_micro_timestep(NewTimeStampTo),

   NewTimeStep =
      case Order of
         asc -> NewTimeStepFrom;
         des -> NewTimeStepTo
      end,

   run_tnesia_query(
      Query#tnesia_query{limit = NewLimit},
      NewTimeStampFrom,
      NewTimeStampTo,
      NewTimeStep,
      NewTimeStepFrom,
      NewTimeStepTo,
      Fun,
      NewState
   );
run_tnesia_query(_, _, _, _, _, _, _, State) -> State.

%%--------------------------------------------------------------------
%% cast_on_bag
%%--------------------------------------------------------------------
cast_on_bag(
   [BagRecord|Tail] = _BagRecords, 
   #tnesia_query{
      limit = Limit,
      from = TimeStampFrom,
      to = TimeStampTo
   } = Query, 
   Fun,
   State
   ) when 
      Limit > 0; 
      Limit =:= unlimited 
         ->

   {Bag, BaseKeyTime} = BagRecord#tnesia_bag.base_key,
   Times = {BaseKeyTime, TimeStampFrom, TimeStampTo},
   case check_timestep_fault(Times) of
      true ->
         [Val] = query_on_frags(
            async_dirty,
            fun() ->
               mnesia:read({
                  tnesia_base,
                  {Bag, BaseKeyTime}
               })
            end
         ),

         NewLimit =
            case apply(Fun, [Val, Limit]) of
               true -> 
                  case Limit of 
                     unlimited -> unlimited;
                     _ -> Limit - 1
                  end
               ;
               _ -> Limit
            end,

         cast_on_bag(Tail, Query#tnesia_query{limit = NewLimit}, Fun, State);
      _ -> cast_on_bag(Tail, Query#tnesia_query{limit = Limit}, Fun, State)
   end;
cast_on_bag(
   _UnwantedBagRecords, 
   #tnesia_query{limit = Limit}, 
   _Fun, 
   State
   ) -> {Limit, State};
cast_on_bag(
   [],
   #tnesia_query{limit = Limit},
   _Fun,
   State
   ) -> {Limit, State}.

%%--------------------------------------------------------------------
%% call_on_bag
%%--------------------------------------------------------------------
call_on_bag(
   [BagRecord|Tail] = _BagRecords, 
   #tnesia_query{
      limit = Limit,
      from = TimeStampFrom,
      to = TimeStampTo
   } = Query, 
   Fun,
   State
   ) when 
      Limit > 0; 
      Limit =:= unlimited 
         ->

   {Bag, BaseKeyTime} = BagRecord#tnesia_bag.base_key,
   Times = {BaseKeyTime, TimeStampFrom, TimeStampTo},
   case check_timestep_fault(Times) of
      true ->
         [Val] = query_on_frags(
            async_dirty,
            fun() ->
               mnesia:read({
                  tnesia_base,
                  {Bag, BaseKeyTime}
               })
            end
         ),

         {NewLimit, NewState} =
            case apply(Fun, [Val, Limit]) of
               {true, Record} -> 
                  case Limit of 
                     unlimited -> {unlimited, lists:append(State, [Record])};
                     _ -> {Limit - 1, lists:append(State, [Record])}
                  end
               ;
               _ -> {Limit, State}
            end,

         call_on_bag(Tail, Query#tnesia_query{limit = NewLimit}, Fun, NewState);
      _ -> call_on_bag(Tail, Query#tnesia_query{limit = Limit}, Fun, State)
   end;
call_on_bag(
   _UnwantedBagRecords, 
   #tnesia_query{limit = Limit}, 
   _Fun, 
   State
   ) -> {Limit, State};
call_on_bag(
   [],
   #tnesia_query{limit = Limit},
   _Fun,
   State
   ) -> {Limit , State}.

%%--------------------------------------------------------------------
%% query_on_frags
%%--------------------------------------------------------------------
query_on_frags(Type, Fun) ->
   mnesia:activity(Type, Fun, [], mnesia_frag).

%%--------------------------------------------------------------------
%% check_timestep_fault
%%--------------------------------------------------------------------
check_timestep_fault({BaseKeyTime, TimeStampFrom, TimeStampTo}) ->
      (BaseKeyTime >= TimeStampFrom) andalso (BaseKeyTime =< TimeStampTo).

%%--------------------------------------------------------------------
%% get_micro_timestamp
%%--------------------------------------------------------------------
get_micro_timestamp(TupleTime) ->
   {Mega, Sec, Micro} = TupleTime,
   SecTimeStamp = (Mega * 1000000 + Sec),
   MicroTimeStamp = (SecTimeStamp * 1000000) + Micro,
   MicroTimeStamp.

%%--------------------------------------------------------------------
%% get_micro_timestep
%%--------------------------------------------------------------------
get_micro_timestep(MicroTimeStamp) ->
   Rem = MicroTimeStamp rem get_micro_timestep_precision(),
   MicroTimeStamp - Rem.

%%--------------------------------------------------------------------
%% get_micro_timestep_precision
%%--------------------------------------------------------------------
get_micro_timestep_precision() ->
   ?TIME_STEP_PERCISION_SEC * 1000000.

%%--------------------------------------------------------------------
%% get_micro_timestamp_count_limit
%%--------------------------------------------------------------------
get_micro_timestamp_count_limit(TupleTime) ->
   get_micro_timestamp(TupleTime) - (?TIME_COUNT_LIMIT_SEC * 1000000).

%%====================================================================
%% Tests
%%====================================================================

%%--------------------------------------------------------------------
%% run_test
%%--------------------------------------------------------------------
run_test() ->
   recreate_table(4),

   T1 = now(),
   seed_sample_data(1000, 2),
   timer:sleep(?TIME_STEP_PERCISION_SEC * 1000),
   _T2 = now(),
   seed_sample_data(1000, 3),
   timer:sleep(?TIME_STEP_PERCISION_SEC * 1000),
   _T3 = now(),
   seed_sample_data(1000, 1),
   timer:sleep(?TIME_STEP_PERCISION_SEC * 1000),
   T4 = now(),

   Bag = <<"id-581">>,

   %% --------------------
   %% --- do functions ---
   %% --------------------

   io:format("~n --- fun: read_since_till_do, order: asc, limit: 5, fun: do --- ~n"),
   read_since_till_do(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 5,
         order = asc
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),

   io:format("~n --- fun: read_since_till_do, order: des, limit: 5, fun: do --- ~n"),
   read_since_till_do(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 5,
         order = des
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),

   io:format("~n --- fun: read_since_till_do, order: des, limit: 3, fun: do --- ~n"),
   read_since_till_do(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 3,
         order = des
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),

   io:format("~n --- fun: read_since_till_do, order: des, limit: un, fun: do --- ~n"),
   read_since_till_do(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = unlimited,
         order = des
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),

   io:format("~n --- fun: read_since_do, order: des, limit: un, fun: do --- ~n"),
   read_since_do(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         limit = unlimited,
         order = des
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),

   io:format("~n --- fun: read_count_do, order: des, limit: 4, fun: do --- ~n"),
   read_count_do(
      #tnesia_query{
         bag = Bag,
         limit = 4
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),

   io:format("~n --- fun: read_count_do, order: asc, limit: 4, fun: do --- ~n"),
   read_count_do(
      #tnesia_query{
         bag = Bag,
         limit = 4,
         order = asc
      },
      fun(Value, Limit) ->
         io:format("v: ~p - l: ~p~n", [Value#tnesia_base.base_key, Limit]),
         true %% because it matched
      end
   ),   

   %% --------------------
   %% --- if functions ---
   %% --------------------

   io:format("~n --- fun: read_since_till_do, order: asc, limit: 5, fun: if --- ~n"),
   Result1 = read_since_till_if(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 5,
         order = asc
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),
   io:format("r: ~p~n", [Result1]),

   io:format("~n --- fun: read_since_till_do, order: des, limit: 5, fun: if --- ~n"),
   Result2 = read_since_till_if(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 5,
         order = des
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),
   io:format("r: ~p~n", [Result2]),

   io:format("~n --- fun: read_since_till_do, order: des, limit: 3, fun: if --- ~n"),
   Result3 = read_since_till_if(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 3,
         order = des
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),
   io:format("r: ~p~n", [Result3]),

   io:format("~n --- fun: read_since_till_do, order: des, limit: un, fun: if --- ~n"),
   Result4 = read_since_till_if(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = unlimited,
         order = des
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),
   io:format("r: ~p~n", [Result4]),

   io:format("~n --- fun: read_since_do, order: des, limit: un, fun: if --- ~n"),
   Result5 = read_since_if(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         limit = unlimited,
         order = des
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),
   io:format("r: ~p~n", [Result5]),

   io:format("~n --- fun: read_count_do, order: des, limit: 4, fun: if --- ~n"),
   Result6 = read_count_if(
      #tnesia_query{
         bag = Bag,
         limit = 4
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),
   io:format("r: ~p~n", [Result6]),

   io:format("~n --- fun: read_count_do, order: asc, limit: 4, fun: if --- ~n"),
   Result7 = read_count_if(
      #tnesia_query{
         bag = Bag,
         limit = 4,
         order = asc
      },
      fun(_Value, _Limit) ->
         true %% because it matched
      end
   ),   
   io:format("r: ~p~n", [Result7]),

   %% --------------------
   %% --- no functions ---
   %% --------------------

   io:format("~n --- fun: read_since_till_do, order: asc, limit: 5, fun: no --- ~n"),
   Result8 = read_since_till(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 5,
         order = asc
      }
   ),
   io:format("r: ~p~n", [Result8]),

   io:format("~n --- fun: read_since_till_do, order: des, limit: 5, fun: no --- ~n"),
   Result9 = read_since_till(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 5,
         order = des
      }
   ),
   io:format("r: ~p~n", [Result9]),

   io:format("~n --- fun: read_since_till_do, order: des, limit: 3, fun: no --- ~n"),
   Result10 = read_since_till(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = 3,
         order = des
      }
   ),
   io:format("r: ~p~n", [Result10]),

   io:format("~n --- fun: read_since_till_do, order: des, limit: un, fun: no --- ~n"),
   Result11 = read_since_till(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         to = get_micro_timestamp(T4),
         limit = unlimited,
         order = des
      }
   ),
   io:format("r: ~p~n", [Result11]),

   io:format("~n --- fun: read_since_do, order: des, limit: un, fun: no --- ~n"),
   Result12 = read_since(
      #tnesia_query{
         bag = Bag,
         from = get_micro_timestamp(T1),
         limit = unlimited,
         order = des
      }
   ),
   io:format("r: ~p~n", [Result12]),

   io:format("~n --- fun: read_count_do, order: des, limit: 4, fun: no --- ~n"),
   Result13 = read_count(
      #tnesia_query{
         bag = Bag,
         limit = 4
      }
   ),
   io:format("r: ~p~n", [Result13]),

   io:format("~n --- fun: read_count_do, order: asc, limit: 4, fun: no --- ~n"),
   Result14 = read_count(
      #tnesia_query{
         bag = Bag,
         limit = 4,
         order = asc
      }
   ),   
   io:format("r: ~p~n", [Result14]),

   done.


