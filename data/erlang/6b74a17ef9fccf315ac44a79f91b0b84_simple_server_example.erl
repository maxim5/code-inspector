%%
%% Simple server:
%% Also see:
%% http://www.erlang.org/doc/man/gen_tcp.html
%%

-module(simple_server_example).
-include("irc_server.hrl").
-export([simple_server_lib/0, start/0, wait_for_messages0/0]).

%%
%% Server handler info:
%% lib_handler - gen_server process id created from invoking server_lib start link.
%%               In the function server_handler, this variable is defined as <P>.
%%               Use lib_handler to pass messages to the server library.
-record(serv_handler_info, {lib_handler}).

%%------------------------------------------------
%% Top-level Handler process, wait for termination
%%------------------------------------------------
start() ->
	% Ref: spawn_link(Module, Function, ArgList)
    spawn_link(?MODULE, wait_for_messages0, []).

%% Simple handler operation handles one message, terminate.
%% The application process will assign this handler for the server library.
wait_for_messages0() ->
	io:format("trace: initialize wait for messages~n"),
	wait_for_messages(idle).

wait_for_messages(idle) ->
	io:format("trace: app:waiting for messages~n"),
	receive
		{ connection_closed } ->
			io:format("trace: app: client connection closed~n");
		{ shutdown } ->			
			io:format("trace: app: shutting down server.~n"),
			% Exit point for the application, full shutdown.
			erlang:halt();
		Else ->
			io:format("trace: app: invalid message~n")
	end.

%%------------------------------------------------
%% Test serverlib
%%------------------------------------------------
server_handler(ServClient, idle) ->
	ServLib = ServClient#serv_handler_info.lib_handler,
	io:format("trace: [!] at server_handler.idle [~p]~n", [ServLib]),  
	io:format("trace: app: wait_messages:accept <incoming>~n"),
    % Launch initial accept clients block
    AcceptCall = server_lib:server_accept_call(ServLib),
	server_handler(ServClient, idle).

%% Simple functional test for the server library
simple_server_lib() ->
	io:format("starting server <with server library support>~n"),
	AppHandler = start(),
	io:format("trace: top-level app handler~p~n", [AppHandler]),
	Client = #irc_server_info{app_handler=AppHandler},
	ServStart = server_lib:start_link(Client),
	case ServStart of
		{ ok, P } ->
			io:format("trace: app:server pid/lib [~p]~n", [P]),
			server_lib:server_listen(P),
			State = server_lib:get_cur_state(P),
			io:format("trace: state [~p]~n", [State]),
			server_handler(#serv_handler_info{lib_handler=P}, idle)			
	end.

%% End of File.
