-module(stream).
-behaviour(gen_server).

-export([test/0]).
-export([start_link/2, make_channel/1, make_sender/2, ready_to_receive/1]).
-export([init/1, terminate/2, handle_info/2, handle_call/3, handle_cast/2, code_change/3]).

run_test(Test) ->
	Me = self(),
	spawn(fun() ->
		Me ! try
			{ok, Test()}
		catch
			throw:Error -> {throw, Error}
		end,
		exit(shutdown)
	end),
	receive
		{ok, Result} -> Result;
		{throw, Error} -> throw(Error);
		{error, Error} -> erlang:error(Error);
		{exit, Error} -> exit(Error)
	end.
	
test() ->
	run_test(fun test_channel_send/0),
	run_test(fun test_performance/0).

test_performance() ->
	{ok, Gateway1} = gateway:start_link(),
	gateway:listen(Gateway1, {127,0,0,1}, 1234),
	{ok, Gateway2} = gateway:start_link(),
	gateway:connect(Gateway2, {127,0,0,1}, 1234, 1),
	{ok, Stream1} = stream:start_link(Gateway1, stream),
	{ok, Stream2} = stream:start_link(Gateway2, stream),
	receive after 500 -> ok end,
	
	Channel = stream:make_channel(Stream1),
	Sender = stream:make_sender(Stream2, Channel),
	
	spawn_link(fun() -> test_performance_send(64 * 1024, 500, Sender) end),
	Start = now(),
	test_performance_recv(Channel),
	End = now(),
	timer:now_diff(End, Start) / 1000000.
	

test_performance_send(_, 0, Sender) -> Sender(close);
test_performance_send(Data, Times, Sender) when is_binary(Data) ->
	%io:format("sending ~p ~p~n", [Times, size(Data)]),
	Sender(Data), test_performance_send(Data, Times - 1, Sender);
test_performance_send(Bytes, Times, Sender) ->
	Bits = Bytes * 8,
	test_performance_send(<<1:Bits>>, Times, Sender).

test_performance_recv(Channel) ->
	ready_to_receive(Channel),
	receive
		{packet, Channel, _Data} ->
			test_performance_recv(Channel);
		{close, Channel} ->
			ok
	end.

test_channel_send() ->
	{ok, Gateway1} = gateway:start_link(),
	gateway:listen(Gateway1, {127,0,0,1}, 1234),
	{ok, Gateway2} = gateway:start_link(),
	gateway:connect(Gateway2, {127,0,0,1}, 1234, 1),
	
	{ok, Stream1} = stream:start_link(Gateway1, stream),
	{ok, Stream2} = stream:start_link(Gateway2, stream),
	
	Channel = stream:make_channel(Stream1),
	
	Sender = stream:make_sender(Stream2, Channel),
	
	Data = <<"this is a test! this is a test! this is a test! this is a test! this is a test! this is a test! ">>,
	Sender(Data),
	Sender(close),
	
	Data = test_recv_loop([], Channel).

test_recv_loop(Acc, Channel) ->
	ready_to_receive(Channel),
	receive
		{packet, Channel, Data} ->
			test_recv_loop([Data|Acc], Channel);
		{close, Channel} ->
			iolist_to_binary(lists:reverse(Acc))
	end.

start_link(Gateway, ServiceName) ->
	gen_server:start_link(?MODULE, [Gateway, {?MODULE, ServiceName}], []).
	
init([Gateway, ServiceName]) ->
	Name = gateway:get_name(Gateway),
	Handler = fun
		(cast, _, {heartbeat, Stream, Pid}) ->
			Pid ! {heartbeat, Stream};
		(cast, From, {packet, Stream, Pid, Sequence, Data, AckPid}) ->
			Pid ! {packet, From, Stream, Sequence, Data, AckPid}; % to receiver_loop()
		(cast, _, {ack, Stream, Pid, Sequence}) ->
			Pid ! {ack, Stream, Sequence}; % to sender_loop()
		(cast, _, {close, Stream, Pid, Sequence}) ->
			Pid ! {close, Stream, Sequence} % to receiver_loop()
	end,
	
	ok = gateway:register(Gateway, ServiceName, Handler),
	RPC_Cast = gateway:cast(Gateway, ServiceName),
	
	{ok, {RPC_Cast, Name}}.
	
receiver_loop(RPC_Cast, Channel, Sequence, ReceiverPid, Active) ->
	{channel, _, Stream, _, _} = Channel,
	InactivityTimeout = 900000, % 90 seconds - three times the 30 second heartbeat
	
	receive
		{heartbeat, Stream} ->
			% reset inactivity timeout
			receiver_loop(RPC_Cast, Channel, Sequence, ReceiverPid, Active);
		
		active ->
			receiver_loop(RPC_Cast, Channel, Sequence, ReceiverPid, active);
			
		{packet, From, Stream, Sequence, Data, AckPid} when Active =:= active ->
			ReceiverPid ! {packet, Channel, Data},
			RPC_Cast(From, {ack, Stream, AckPid, Sequence}),
			receiver_loop(RPC_Cast, Channel, Sequence + 1, ReceiverPid, inactive);
		{packet, _, Stream, S, _, _} when S < Sequence ->
			drop_duplicate,
			receiver_loop(RPC_Cast, Channel, Sequence, ReceiverPid, Active);
		
		{close, Stream, Sequence} when Active =:= active ->
			ReceiverPid ! {close, Channel},
			closed
	after InactivityTimeout ->
		timeout % just give up. the remote end will give up eventually
	end.

sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, AckSequence) ->
	receive
		{chunk, _, _, 0} ->
			timeout_send; % give up on the chunk, and terminate the sender
		{chunk, _, ChunkSequence, _} when ChunkSequence =< AckSequence ->
			sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, AckSequence);
		{chunk, Chunk, ChunkSequence, Retry} when ChunkSequence < AckSequence + 10 -> % window size
			RPC_Cast(Name, {packet, Stream, Pid, ChunkSequence, Chunk, self()}),
			timer:send_after(2000, {chunk, Chunk, ChunkSequence, Retry-1}), % schedule a retry
			sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, AckSequence);
		
		{ack, Stream, Seq} when Seq > AckSequence ->
			sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, Seq);
		{ack, _, _} ->
			sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, AckSequence); % ignore out-of-sequence acks
			
		{data, Data, _SendingPid} ->
			NewSequence = chunkify(Data, Sequence),
			sender_loop(RPC_Cast, Name, Stream, Pid, NewSequence, AckSequence);
		
		close when AckSequence >= Sequence - 1 ->
			RPC_Cast(Name, {close, Stream, Pid, Sequence});
		{'EXIT', _, _} ->
			RPC_Cast(Name, {close, Stream, Pid, Sequence});
		
		heartbeat ->
			RPC_Cast(Name, {heartbeat, Stream, Pid}),
			timer:send_after(30000, heartbeat),
			sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, AckSequence)
	end.

% Break a big binary into smaller chunks for sending.
send_chunk(Chunk, Rest, Sequence) ->
	self() ! {chunk, Chunk, Sequence, 60},
	chunkify(Rest, Sequence + 1).
chunkify(<<>>, Sequence) -> Sequence;
chunkify(<<Chunk:(64 * 1024)/binary, Rest/binary>>, Sequence) -> % 64K "packets" turn out pretty well (in loopback tests, at least)
	send_chunk(Chunk, Rest, Sequence);
chunkify(Chunk, Sequence) ->
	send_chunk(Chunk, <<>>, Sequence).


% Turn a channel that was send from remotely into a function that can be used to send data to that channel.
make_sender(Server, Channel) ->
	gen_server:call(Server, {make_sender, Channel}).

% Returns a new Channel.
make_channel(Server) ->
	gen_server:call(Server, {make_stream, self()}).
	
ready_to_receive({channel, _, _, StreamPid, _}) ->
	StreamPid ! active.

handle_call(Request, _Client, State) ->
	{RPC_Cast, MyName} = State,
	case Request of
		{make_stream, Pid} ->
			Stream = guid:new(),
			InitSequence = 1,
			StreamPid = spawn_link(fun() ->
				receiver_loop(RPC_Cast, {channel, MyName, Stream, self(), InitSequence}, InitSequence, Pid, inactive)
			end),
			Channel = {channel, MyName, Stream, StreamPid, InitSequence},
			{reply, Channel, State};
		
		{make_sender, {channel, Name, Stream, Pid, Sequence}} ->
			SenderPid = spawn_link(fun() ->
				process_flag(trap_exit, true),
				timer:send_after(5000, heartbeat),
				sender_loop(RPC_Cast, Name, Stream, Pid, Sequence, Sequence - 1)
			end),
			Sender = fun
				(close) -> SenderPid ! close;
				(Data) when is_binary(Data) ->
					SenderPid ! {data, Data, self()}
			end,
			
			{reply, Sender, State}
	end.
	
handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.
	
terminate(_Reason, _State) ->
	ok.

code_change(_Version, State, _Extra) -> {ok, State}.