%% @doc The interface to the game server. Handles all the communication between the modules and the game server.
%% @end
-module(interface).
-export([init/0,interface/2]).

%% @doc  Starts interface/2 as a new process
%% @spec init() -> Pid
%% @end  
init() ->
	spawn(fun() -> interface([],[]) end).

%% @doc Handles the communication with the game server through a socket. When a message is received from the server it parses it and sends the parsed message to the actuator. It also receives messages from msg that it passes forward to the server 
%% @end  
interface(Sock,Pids) ->
	receive
		{initIDs, IDs} -> 
			{ok, NSock} = gen_tcp:connect("localhost", 8880,[binary, {packet, 2},{active,true}]),
			interface(NSock,IDs);
        {tcp, Sock, B} ->
			{A,C} = parser:binmsg_to_tup(B),
			%DEBUG writes: Type | , arg1,arg2 ... to stdout
            L = lists:foldl(fun(X,Y) -> Y ++ [","] ++ [X] end, [], C),
			%io:fwrite("~s~s~n",[A,lists:concat([" | "]++L)]),
			{actuator,APid} = lists:keyfind(actuator,1,Pids),
			APid ! {message,A,C},
			interface(Sock,Pids);			
		{smessage, Bin} -> 
			io:fwrite("~s~n",[Bin]),
			gen_tcp:send(Sock,Bin),
			interface(Sock,Pids);
		{error, Reason} ->  
			io:fwrite("~s~n", [Reason]),
			interface(Sock,Pids);
		{tcp_closed,Sock} ->
			io:fwrite("Connection closed!~nExiting interface!~n");
		X               ->
			io:fwrite("~w~n~s~n",[X,"kom in fel i interface"]),
			interface(Sock,Pids)
	end.
