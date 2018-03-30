%%
%% Example eunit test case.
%% http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html
%%
%% Also see:
%% http://www.irchelp.org/irchelp/rfc/rfc.html
%% An IRC (Internet Relay Chat) message is composed of 
%% fields delimited by a space, up to an optional last 
%% argument which may include spaces
-module(test_parse_data).
-export([run_tests/0]).
-include_lib("eunit/include/eunit.hrl").

-import(data_lib).

data_lib_test_() ->
	Data1 = data_lib:strip_lineend("this is a test\r\n"),
	Data2 = data_lib:parse_args("PRIVMSG the args"),
	Data3 = data_lib:scan_string(":TheSender PRIVMSG TheTo :how is it going\r\n"),
	[
	 ?_assert(Data1 == "this is a test"),
	 ?_assert(Data2 == ["PRIVMSG","the","args"]),
	 ?_assert(Data3 == {"TheSender","PRIVMSG",
						["TheTo","how is it going"]})
	].

run_tests() ->
	eunit:test(test_parse_data).
%% End of file
