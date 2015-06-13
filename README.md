Tnesia
======

Tnesia is a time-series data storage based on Mnesia. You can run time-based queries on a large amount of data, without scanning the whole set of data, in a key-value manner.

Goal and Theory
-----
The goal is to reduce disk seeks in order to find a time range of data values in a timeline, ascending or descending, and from or to any arbitrary points of time.
There are few terms that can help you to understand how Tnesia stores and retrieves data, among them **Timeline**, **Timepoint** and **Timestep** are the main ones.


```
    +---------------------------+        
    |                           |        
    |     + timepoint # 01      |        
    |                           |        
    |                                          - timeline:
    |     + timepoint # 02   timestep # 1        an identifier to a
    |                                            series of chronological
    |     + timepoint # 03      |                timepoints
    |     + timepoint # 04      |        
    |                           |        
    +---------------------------+              - timepoint:
    |                           |                a pointer to a given
    |     + timepoint # 05      |                data value
    |     + timepoint # 06      |        
                                         
timeline  + timepoint # 07   timestep # 2      - timestep:
                                                 a partition on a
    |     + timepoint # 08      |                timeline that spilited
    |     + timepoint # 09      |                timepoints
    |                           |        
    +---------------------------+        
    |                           |        
    |     + timepoint # 10      |        
    |                           |        
    |     + timepoint # 11               
    |     + timepoint # 12   timestep # 3
    |                                    
    |                           |        
    |     + timepoint # 13      |        
    |                           |        
    +---------------------------+        
    |                           |        
    v                           v        
```

**Quick Facts**:

* Each timestep can contain arbitrary number of timepoints.
* All the orders are based on microsecond UNIX timestamp.
* Timepoints are just a pointer to its data value which stores somewhere else.
* Timepoints, as index, are stored on RAM to faster lookup.
* Data values are stored on Disk to have more room for storage.
* All seeks are **key-value**.
* Timestep precision is configurable depends on the timepoints frequency.

API
-----

**Write**

Writes a record on a timeline.

```erlang
tnesia_api:write(Timeline, Record) -> {Timeline, Timepoint}
```

**Read**

Reads a record from a timeline by its timepoint.

```erlang
tnesia_api:read(Timeline, Timepoint) -> Record
```

**Remove**

Removes a record from a timeline by its timepoint.

```erlang
tnesia_api:remove(Timeline, Timepoint) -> ok
```

**Query fetch**

Queries a timeline to return records without any filttering or mapping.

```erlang
tnesia_api:query_fetch(Query) -> [Record]
```

**Query filtermap**

Queries a timeline to return records with filltering and mapping.

```erlang
tnesia_api:query_filtermap(Query, FiltermapFun) -> [Record]
```

**Query foreach**

Queries a timeline and traverse on the records.

```erlang
tnesia_api:query_foreach(Query, ForeachFun) -> ok
```

**Query raw**

Queries a timeline deciding on the return and traversal method.

```erlang
tnesia_api:query_raw(Query, Return, Fun) -> [Record] | ok
```

Types
----

```erlang
Timeline = Record = any()
Timepoint = integer()
Query = [{timeline, Timeline},
         {since, Since},
         {till, Till},
         {order, Order},
         {limit, Limit}]
Since = Till = integer()
Order = asc | des
Limit = integer() | unlimited
FiltermapFun = fun((Record, RecordIndex, Limit) -> {true, Record} | false)
ForeachFun = fun((RecordIndex, Record, Limit) -> true | any())
Fun = FiltermapFun | ForeachFun
Return = true | false
```

Examples
----

* Writing a tweet on a user's timeline.

```erlang
-define(timeline, {user}).
-define(tweet, {text, media}).

Timeline = #timeline{user = "@joeerl"},
Tweet = #tweet{text = "Do not break the laws of physics", media = null},
{Timeline, Timepoint} = tnesia_api:write(Timeline, Tweet).
```

* Finding at most 10 tweets of a given user in a specific range of time with ascending order.

```erlang
-define(timeline, {user}).

Since = tnesia_lib:get_micro_timestamp({2015, 1, 1}, {0, 0, 0}),
Till = tnesia_lib:get_micro_timestamp(({2015, 2, 1}, {0, 0, 0}),
Timeline = #timeline{user = "@joeerl"},
Result = tnesia_api:query_fetch([
				 {timeline, Timeline},
				 {since, Since},
				 {till, Till},
				 {order, asc},
				 {limit, 10}
				]).

```

* Finding at most 5 attached media which a given user tweeted in a specific range of time.

```erlang
-define(timeline, {user}).
-define(tweet, {text, media}).

Since = tnesia_lib:get_micro_timestamp({2015, 1, 1}, {0, 0, 0}),
Till = tnesia_lib:get_micro_timestamp({2015, 2, 1}, {0, 0, 0}),
Timeline = #timeline{user = "@joeerl"},
Result = tnesia_api:query_filtermap([
				     {timeline, Timeline},
				     {since, Since},
				     {till, Till},
				     {order, des},
				     {limit, 10}
				    ],
				    fun(Record, _RecordIndex, _RemainingLimit) ->
					    case Record#tweet.media of
						null -> false;
						_ -> {true, Record#tweet.media}
					    end
				    end).
```

* Removing 1000 tweets of a given user in a specific range of time whose size is more that 100 characters.

```erlang
-define(timeline, {user}).
-define(tweet, {text, media}).

Since = tnesia_lib:get_micro_timestamp({2014, 1, 1}, {0, 0, 0})
Till = tnesia_lib:get_micro_timestamp({2015, 1, 1}, {0, 0, 0}),
Timeline = #timeline{user = "@joeerl"},
ok = tnesia_api:query_foreach([
			       {timeline, Timeline},
			       {since, Since},
			       {till, Till},
			       {limit, 1000}
			      ],
			      fun(Record, RecordIndex, _RemainingLimit) ->
				      {_, _, {Timeline, Timepoint}} = RecordIndex,
				      if
					  length(Record#tweet.text) > 100 ->
					      tnesia_api:remove(Timeline, Timepoint),
					      true;
					  true  -> false
				      end,
			      end).
```

Contribute
----

Comments, contributions and patches are greatly appreciated.