-type tnesia_todo() :: any().
-type tnesia_timeline() :: any().
-type tnesia_timepoint() :: integer().

-type tnesia_record() :: tnesia_todo().
-type tnesia_index() :: tnesia_todo().

-type tnesia_base() :: tnesia_record().
-type tnesia_bag() :: tnesia_index().

-type tnesia_query() :: [{atom(), any()}].
-type tnesia_since() :: integer().
-type tnesia_till() :: integer().
-type tnesia_order() :: asc | desc.
-type tnesia_limit() :: integer() | unlimited.
-type tnesia_return() :: true | false.

-type tnesia_filtermap_fun() :: 
	fun((tnesia_record(), tnesia_index(), tnesia_limit()) -> 
		   {true | tnesia_record()} | false).
-type tnesia_foreach_fun() ::
	fun((tnesia_record(), tnesia_index(), tnesia_limit()) ->
		   true | any()).
-type tnesia_fun() :: tnesia_filtermap_fun() | tnesia_foreach_fun().
