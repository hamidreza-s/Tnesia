-module(tnesia_http_tql).

-behaviour(supervisor).

-export([
	 start_link/0,
	 add_listener/0,
	 init/1
	]).

-export([
	 listen/1
	]).

-include("tnesia.hrl").

%%====================================================================
%% Main API
%%====================================================================

%%--------------------------------------------------------------------
%% start_link
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_listener() ->
    supervisor:start_child(?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% init
%%--------------------------------------------------------------------
init([]) ->

    Port = ?CONFIG_HTTP_TQL_PORT,
    IP = ?CONFIG_HTTP_TQL_IP,
    {ok, ListeningSock} = 
	gen_tcp:listen(Port,
		       [binary,
			{active, once},
			{ip, IP}]),
    
    {ok, {{simple_one_for_one, 5, 10},
	  [
	   {?MODULE, 
	    {?MODULE, listen, [ListeningSock]},
	    permanent,
	    3000,
	    worker, 
	    [?MODULE]}]
	 }}.

%%====================================================================
%% Listener Loop
%%====================================================================

%%--------------------------------------------------------------------
%% listen
%%--------------------------------------------------------------------
listen(ListeningSock) ->
    {ok, 
     spawn_link(fun() ->
			{ok, Sock} = gen_tcp:accept(ListeningSock),
			request_handler(Sock)
		end)}.

%%--------------------------------------------------------------------
%% request_handler
%%--------------------------------------------------------------------
request_handler(Sock) ->
    receive
	{tcp, Sock, Data} ->
	    Request = binary:split(Data, <<"\r\n">>, [global]),
	    response_handler(Request, Sock);
	{tcp_closed, Sock} ->
	    gen_tcp:close(Sock)
    end.

%%--------------------------------------------------------------------
%% request_handler
%%--------------------------------------------------------------------
response_handler(Request, Sock) ->
    response_handler(Request, Sock, []).
response_handler([<<"Query: ", Query/binary>> | Request], Sock, State) ->
    response_handler(Request, Sock, [{query, Query} | State]);
response_handler([<<"Accept: ", Accept/binary>> | Request], Sock, State) ->
    response_handler(Request, Sock, [{accept, Accept} | State]);
response_handler([_|Request], Sock, State) ->
    response_handler(Request, Sock, State);
response_handler([], Sock, State) ->
    do_response(State, Sock).

do_response(Request, Sock) ->
    Query = binary_to_list(?LOOKUP(query, Request)),
    _Accept = ?LOOKUP(accept, Request),
    Accept = json, %% @TODO: make it dynamic
    ContentType = "application/json", %% @TODO: make it dynamic
    Result = ?TQL_API:query_format(Query, Accept),

    Response = 
	iolist_to_binary(
	  io_lib:fwrite("HTTP/1.0 200 OK\n" ++ 
			    "Content-Type: ~s\n" ++ 
			    "Content-Length: ~p\n\n~s",
			[ContentType, 
			 size(Result), 
			 Result])),
    
    gen_tcp:send(Sock, Response),

    ok.
    


