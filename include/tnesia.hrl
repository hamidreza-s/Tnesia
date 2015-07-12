%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(tnesia_input, {timeline, timepoint, record}).
-record(tnesia_base, {base_key, base_val}).
-record(tnesia_bag, {bag_key, base_key}).
-record(tnesia_sample, {foo, bar, bat}).
-record(tnesia_query, {
	  bag :: any(),
	  from :: integer(),         %% micro timestamp 
	  to :: integer(),           %% micro timestamp
	  limit = 50 :: integer(),
	  return :: true | false,
	  order = asc :: asc | des
	 }).

%%--------------------------------------------------------------------
%% Names
%%--------------------------------------------------------------------
-define(API, tnesia_api).
-define(LIB, tnesia_lib).
-define(TQL_API, tnesia_tql_api).
-define(TQL_SCANNER, tnesia_tql_scanner).
-define(TQL_PARSER, tnesia_tql_parser).
-define(TQL_EVALUATOR, tnesia_tql_evaluator).
-define(TQL_LINTER, tnesia_tql_linter).
-define(TQL_FORMATTER, tnesia_tql_formatter).

%%--------------------------------------------------------------------
%% Configs
%%--------------------------------------------------------------------
-define(CONFIG_HTTP_TQL_PORT, 1881).
-define(CONFIG_HTTP_TQL_IP, {127, 0, 0, 1}).
-define(CONFIG_HTTP_TQL_LISTENERS, 5).

%%--------------------------------------------------------------------
%% Macro Functions
%%--------------------------------------------------------------------
-define(DBG_CT(Format, Args),
	ct:print(default, 50, Format, Args)).
-define(DBG(Format, Args), 
	error_logger:info_msg(Format, Args)).
-define(LOOKUP(Key, List),
	proplists:get_value(Key, List)).
-define(FORMAT(Str, Arg),
	lists:flatten(io_lib:format(Str, Arg))).
-define(TYPE(Term),
	tnesia_util:type_of(Term)).
-define(TO_LIST(Term),
	tnesia_util:to_list(Term)).
-define(TO_BIN(Term),
	tnesia_util:to_binary(Term)).
-define(TRIM_STR(Direction, Count, String),
	tnesia_util:trim_string(Direction, Count, String)).
-define(IS_PROPLISTS(Term),
	tnesia_util:is_proplists(Term)).
