%%
%% Example eunit test case.
%% http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html
%%
%% Also see:
%% http://www.irchelp.org/irchelp/rfc/rfc.html
%% An IRC (Internet Relay Chat) message is composed of 
%% fields delimited by a space, up to an optional last 
%% argument which may include spaces
-module(test_shutdown_server).
-export([run_tests/0]).
-include_lib("eunit/include/eunit.hrl").

-import(data_lib).

do_recv(Sock, Bs) ->
	case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

simple_server_test_() ->
	SomeHostInNet = "127.0.0.1",
	{ok, Sock} = gen_tcp:connect(SomeHostInNet, 9003, 
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "sysquit\r\n"),	
	[
	].

run_tests() ->
	eunit:test(test_shutdown_server).
%% End of file
