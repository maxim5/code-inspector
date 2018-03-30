%%%-------------------------------------------------------------------
%%% @author dean <wagner@nmb.gov>
%%% @copyright (C) 2012, dean wagner
%%% @doc
%%% This is a facade for Google App Engine.
%%% @end
%%% Created : 25 Jul 2012 by dean <wagner@nmb.gov>
%%%-------------------------------------------------------------------
-module(as_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 get_table/2,
	 put_table/3,
	 get_list_of_tables/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Sends a table of Entity to an app located at Url.  Data is a list
%% of JSON strings for each entity.
%%
%% @spec put_table(Url, Entity, Data) -> {ok, {Url, Entity}} | 
%%                                      {error, Reason}
%% @end
%%--------------------------------------------------------------------
put_table(Url, Entity, Data) -> 
    gen_server:call(?MODULE, {put, Url,Entity, Data},6000000).

%%--------------------------------------------------------------------
%% @doc
%% Gets a table of Entity from an app located at Url.  Data is a list
%% of JSON strings for each entity.
%%
%% @spec get_table(Url, Entity) -> {ok, {Url, Entity, Data}} | 
%%                               {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_table(Url, Entity) -> 
    gen_server:call(?MODULE, {get, Url,Entity},6000000).

%%--------------------------------------------------------------------
%% @doc
%% Gets a table of Entity from an app located at Url.  Data is a list
%% of JSON strings for each entity.
%%
%% @spec get_list_of_tables(Url) -> {ok, {Url, ListOfTables}} | 
%%                               {error, Reason}

%% @end
%%--------------------------------------------------------------------
get_list_of_tables(Url) -> 
    gen_server:call(?MODULE, {list, Url}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}, infinity}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling put_table requests
%%
%% @spec handle_call({put, _Url, _Entity, _Data}, _From, State) ->
%%                   {reply, Reply, State}
%% @end
%%--------------------------------------------------------------------
handle_call({put, Url, Entity, Data}, _From, State) ->
    error_logger:error_msg("I am putting ~p~n",[Entity]),
    PutUrl = Url ++ "/" ++ Entity,
    inets:start(),
    try
	lists:foreach(fun(X) -> post1(self(), 
				      "dean", 
				      "dean", 
				      X, 
				      PutUrl) 
		      end, Data)
    catch
	_:_ -> error_logger:error_msg("~n Failure to appengine write")
    end,
    inets:stop(),
    Reply = Entity,
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling get_table requests.  Url is a full url including the 
%% servlet name. A slash is added and then the Entity (which should be 
%% name of the table
%%
%% @spec handle_call({get, _Url, _Entity}, _From, State) ->
%%                   {reply, Reply, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Url, Servlet}, _From, State) ->
    inets:start(),
    GetUrl = Url ++ "/" ++ Servlet,
   {ok,{{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
	        httpc:request(get,{GetUrl,[]}, [], []),
    inets:stop(),
    Reply = {Url, Servlet, Body},
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets a list of available tables from the app.  assumes that such
%% a function is bound to Url.
%%
%% @spec handle_call({get_list_of_tables, _Url}, _From, State) ->
%%                   {reply, Reply, State}
%% @end
%%--------------------------------------------------------------------
handle_call({list, _Url}, _From, State) ->
    Reply = "This is a list request",
    {reply, Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Catch-all default clause
%%
%% @spec handle_call(_Request, _From, State) ->
%%                   {reply, Reply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, "danger", State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% requires R15B and json2


% Msg is a tuple {struct,[{key,value},... ]}
post1(PID, ID, PW, Msg, Url) -> 
    {struct,YoArray} = Msg,
    EncodeMsg = jsonarray_to_list(YoArray),
    {ok,{{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = 
	httpc:request(post, 
		      {Url, 
		      [], 
		      "application/x-www-form-urlencoded",
		      url_encode([{"userID", ID}, 
				  {"userPW", PW}, 
				  {"msg", EncodeMsg}])}, 
		      [], []),
    PID ! {ok}.



url_encode(Data) ->
    url_encode(Data,"").

url_encode([],Acc) ->
    Acc;
url_encode([{Key,Value}|R],"") ->
    url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).

jsonarray_to_list(JsonArry) ->
    jsonarray_to_list(JsonArry,"").

jsonarray_to_list([],Acc) ->
    Acc ++ "}}";
jsonarray_to_list([{Key,Value}|R],"") ->
    jsonarray_to_list(R, "{\"entity\":{\"" ++ Key ++ "\":\"" ++ Value ++ "\"");
jsonarray_to_list([{Key,Value}|R],Acc) ->
    jsonarray_to_list(R, Acc ++ ",\"" ++ Key ++ "\":\"" ++ Value ++ "\"").
