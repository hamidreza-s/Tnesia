Tnesia
======

Tnesia is a time-series data storage which lets you run time-based queries on a large amount of data, without scanning the whole set of data, and in a key-value manner. It can be used embeded inside an Erlang application, or stand-alone with HTTP interface to outside which talks in a simple query language called TQL.

Quick Start
-----

**Installation**

What you need to install Tnesia is Erlang/OTP R15 or newer.
Clone the repo and make it before start as follows:

```bash
$ git clone https://github.com/bisphone/Tnesia.git
$ cd Tnesia
$ make
$ make start
#=> Tnesia was started!
```

**Stand-alone Example**

In stand-alone mode, what you need is an HTTP client, like curl. Then you can manipulate time-series data using TQL.
For example you can create a timeline which is called 'tweets' and insert tweets on it in single mode:

```bash
$ curl localhost:1881 -H \
       "Query: INSERT INTO 'tweets' {'text', 'media', 'length'}
               RECORDS {'Hi Tnesia!', 'tnesia.png', '10'}"
#=> [
#=>     1436699673792910
#=> ]
```


Or in batch mode:

```bash
$ curl localhost:1881 -H \
       "Query: INSERT INTO 'tweets' {'text', 'media', 'length'}
               RECORDS {'TQL is simple', 'null', '13'}
               AND {'Erlang is totally fun.', 'erlang.png', '22'}
               AND {'OH: Reactive!', 'null', '13'}"
#=> [
#=>     1436699709083018,
#=>     1436699709082909,
#=>     1436699709082768
#=> ]
```

Now we can select last two tweets as follows:

```bash
$ curl localhost:1881 -H \
       "Query: SELECT * FROM 'tweets'
               WHERE SINCE '1436699673792910' TILL '1436699709083018'
               AND LIMIT '2'
               AND ORDER DES" 
#=> [
#=>     {
#=>         "__timeline__": "tweets",
#=>         "__timepoint__": "1436699709082909",
#=>         "length": "22",
#=>         "media": "erlang.png",
#=>         "text": "Erlang is totally fun."
#=>     },
#=>     {
#=>         "__timeline__": "tweets",
#=>         "__timepoint__": "1436699709083018",
#=>         "length": "13",
#=>         "media": "null",
#=>         "text": "OH: Reactive!"
#=>     }
#=> ]
```

Also selecting tweets which don't have media property with more than 20 text length is as simple as follows:

```bash
$ curl localhost:1881 -H \
       "Query: SELECT {'text', 'media'} FROM 'tweets'
               WHERE 'media' != 'null'
               AND 'length' > '20'"
#=> [
#=>     {
#=>         "__timeline__": "tweets",
#=>         "__timepoint__": "1436699709082909",
#=>         "media": "erlang.png",
#=>         "text": "Erlang is totally fun."
#=>     }
#=> ]
```

**Embeded Example**

The other way to use Tnesia is from inside an Erlang application. It is a standard OTP application, so can be included in Rebar config file.
For example repeating above examples are as follows:

Inserting record in a timeline.

```erlang
Timeline = "tweets",
Tweet = [{"text", "Hi Tnesia!"}, {"media", "tnesia.png"}, {"length", "10"}],
{Timeline, Timepoint} = tnesia_api:write(Timeline, Tweet),
```

Selecting records without filtering and mapping them.

```erlang
Timeline = 'tweets',
Since = tnesia_lib:get_micro_timestamp({2015, 1, 1}, {0, 0, 0}),
Till = tnesia_lib:get_micro_timestamp(({2015, 2, 1}, {0, 0, 0}),
Result = tnesia_api:query_fetch([{timeline, Timeline},
             {since, Since},
             {till, Till},
             {order, asc},
             {limit, 10}]),
```

Selecting text property of records whose media property is null.

```erlang
Since = tnesia_lib:get_micro_timestamp({2015, 1, 1}, {0, 0, 0}),
Till = tnesia_lib:get_micro_timestamp({2015, 2, 1}, {0, 0, 0}),
Timeline = "tweets",
Result = tnesia_api:query_filtermap(
             [{timeline, Timeline},
              {since, Since},
              {till, Till},
              {order, des},
              {limit, 10}],
              fun(Record, _RecordIndex, _RemainingLimit) ->
                  case proplists:get_value("media", Record) of
                      "null" -> false;
                      _ -> {true, proplists:get_value("text", Record)}
                  end
              end).
```

Deleting tweets whose text length is more than 20 characters.

```erlang
Since = tnesia_lib:get_micro_timestamp({2015, 1, 1}, {0, 0, 0}),
Till = tnesia_lib:get_micro_timestamp({2015, 2, 1}, {0, 0, 0}),
Timeline = "tweets",
ok = tnesia_api:query_foreach(
         [{timeline, Timeline},
          {since, Since},
          {till, Till},
          {order, des},
          {limit, 10}],
          fun(Record, RecordIndex, _RemainingLimit) ->
              {_, _, {Timeline, Timepoint}} = RecordIndex,
              Text = proplists:get_value("text", Record),
              case length(Text) > 20 of
                  true -> 
                      tnesia_api:remove(Timeline, Timepoint),
                      true;
                  _ -> false
              end
          end).
```

Theory and Goal
-----

The goal is to reduce disk seeks in order to find a time range of data values in a timeline, ascending or descending, and from or to any arbitrary points of time.
There are few terms that can help you to understand how Tnesia stores and retrieves data, among them **Timeline**, **Timepoint** and **Timestep** are the main ones.

* Timeline: An identifier to a series of chronological timepoints.
* Timepoint: A pointer to a given data value.
* Timestep: A partition on a timeline that spilited timepoints

```
          TL: Timeline     TS: Timestep     *: Timepoint    
                                                            
    +------------------------------------------------------+
TL     * * **  |*  ** *   |*   *  *  |  ***   * |    * ** * 
    +------------------------------------------------------+
        TS         TS         TS         TS         TS      
```

* Each timestep can contain arbitrary number of timepoints.
* All the orders are based on microsecond UNIX timestamp.
* Timepoints are just a pointer to its data value which stores somewhere else.
* Timepoints, as index, are stored on RAM to faster lookup.
* Data values are stored on Disk to have more room for storage.
* All seeks are **key-value**.
* Timestep precision is configurable depends on the timepoints frequency.

Makefile
-----

The Makefile which is in root directory performs the following tasks:

```shell
# building
$ make

# functionality testing
$ make test

# benchmark testing
$ make light_bench
$ make normal_bench
$ make heavy_bench

# starting and etc
$ make start
$ make attach
$ make stop
$ make live
```

TQL API
-----

**Select**

```sql
SELECT * | record_keys()
FROM timeline()
[ WHERE
   [ [ AND ] conditions() ]
   [ [ AND ] SINCE datetime() TILL datetime() ]
   [ [ AND ] ORDER order() ]
   [ [ AND ] LIMIT limit() ] ]
```

**Insert**

```sql
INSERT INTO timeline() record_keys()
RECORDS record_values()
```

**Delete**

```sql
DELETE FROM timeline()
WHEN record_time()
```

TQL Types
----

```sql
timeline() :: 'string()'
record_keys() :: {'string()', ...}
record_values() :: {'string()', ...}
record_time() :: 'string()'
datetime() :: 'integer()'
order() :: 'asc' | 'des'
limit() :: 'integer()'
conditions() :: condition() AND condition()
condition() :: 'string()' comparator() 'string()'
comparator() :: == | != | > | >= | < | <=
```

Erlang Query API
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

Erlang Query Types
----

```erlang
Timeline :: any()
Record :: [{any(), any()}]
Timepoint :: integer()
Query :: [{timeline, Timeline},
           {since, Since},
           {till, Till},
           {order, Order},
           {limit, Limit}]
Since = Till :: integer()
Order :: asc | des
Limit :: integer() | unlimited
FiltermapFun :: fun((Record, RecordIndex, Limit) -> {true, Record} | false)
ForeachFun :: fun((RecordIndex, Record, Limit) -> true | any())
Fun :: FiltermapFun | ForeachFun
Return :: true | false
```

Contribution
-----

Comments, contributions and patches are greatly appreciated.

License
-----
The MIT License (MIT).
