-module(example).
-behaviour(gen_server). 

-export([start_link/0]).

-export([init/1, handle_call/3]).

-include("rpc.hrl").

uuid(N)->
	<<H1:32,H2:16,H3:16,H4:16,H5:48,_/binary>> = crypto:sha(term_to_binary({N})),
	lists:flatten(io_lib:format( 
			"~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",[H1,H2,H3,H4,H5])).

start_link()->
	{ok,Pid} = gen_server:start_link(?MODULE, [], []),
	Service = #service{
			name="example",
			version="1.0",
			handler={pid,Pid},
			procs=[#service_proc{name = "uuid",
                                 	     params = []},
			       #service_proc{name = "echo",
                                 	     params = [{"x","str"}]}
			]
	},
	minitrue_rpc_registry:register_service(Service),
	{ok,Pid}.

init([])->
	{ok,[]}.

handle_call({jsonrpc, "uuid", _,_},_,S) ->
	{reply,{result,uuid({make_ref(),now()})},S};

handle_call({jsonrpc, "echo", [X],RequestInfo},_,S) -> 
	{reply,{result,{struct, [{x,X},{request,lists:flatten(io_lib:format("~p",[RequestInfo]))}]}},S}.
