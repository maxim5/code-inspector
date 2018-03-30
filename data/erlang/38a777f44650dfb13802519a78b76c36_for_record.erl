%% Simple test of using record
%% Also note, ever character and CASE is important
-module(for_record).
-export([main/0]).

%% Simple record definition
-record(the_record, {
		  xkey = 1.0,
		  ykey = 3.0,
		  buffer = "dogs and cats",
		  status = not_done_yet,
		  got_update
		  }).

simple_record_test(OrigRecord) ->
	XVal = OrigRecord#the_record.xkey,
	XBuf = OrigRecord#the_record.buffer,
	%% Update a value
	R2 = OrigRecord#the_record{got_update="MyData"},
	io:format("New Record =>~p~n", [R2]),
	%% Return an updated record
	OrigRecord#the_record{buffer="hahah"}.
	
with_pattern_match(#the_record{buffer=Buf} = Rcrd) ->
	io:format("==> Buffer: ~p~n", [Buf]),
	io:format("==> Record: ~p~n", [Rcrd]),
	Rcrd.

common_idiom_tupl(#the_record{buffer=Buf} = Rcrd) ->
	%% A common Erlang pattern, return a TUPLE with the
	%% status of the function call.  Not an ATOM is
	%% set to the first value of the tuple.
	{ ok_atom, Rcrd }.
	
main() ->
	FirstRec = #the_record{status=new_rec},
	io:format("First Record =>~p~n", [FirstRec]),
	%% Notice that we are passing the updated records to the
	%% next function call.
	R2 = simple_record_test(FirstRec),
	R3 = with_pattern_match(R2),
	R4WithTuple = common_idiom_tupl(R3),
	io:format("Record AND Tuple =>~p~n", [R4WithTuple]).

