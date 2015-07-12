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
                      _ -> {true, proplists:get_value(Record)}
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
                      tnesia_api:remove(Timeline, Timepoint,
                      true;
                  _ -> false
              end
          end).
```

Documentation
-----

* [Theory and Goal](#)
* [Installation and Configuration](#)
* [Functionality Test](#)
* [Benchmark Test](#)
* [Interfaces](#)
    * [TQL API](#)
    * [Erlang API](#)

Contribution
-----

Comments, contributions and patches are greatly appreciated.

License
-----
The MIT License (MIT).
