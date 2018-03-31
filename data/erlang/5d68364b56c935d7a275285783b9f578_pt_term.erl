%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(pt_term).

%% API
-export([
	to_binary/1,
	to_list/1
]).

%% ============================================================================
%% API
%% ============================================================================

-spec to_binary(binary() | string() | atom() | integer() | float()) -> binary().
to_binary(Val) when is_binary(Val)  -> Val;
to_binary(Val) when is_list(Val)    -> erlang:list_to_binary(Val);
to_binary(Val) when is_atom(Val)    -> erlang:atom_to_binary(Val, utf8);
to_binary(Val) when is_integer(Val) -> erlang:integer_to_binary(Val);
to_binary(Val) when is_float(Val)   -> erlang:float_to_binary(Val);
to_binary(_)                        -> erlang:throw(badarg).

-spec to_list(binary() | string() | atom() | integer() | float()) -> list().
to_list(Val) when is_list(Val)    -> Val;
to_list(Val) when is_binary(Val)  -> erlang:binary_to_list(Val);
to_list(Val) when is_atom(Val)    -> erlang:atom_to_list(Val);
to_list(Val) when is_integer(Val) -> erlang:integer_to_list(Val);
to_list(Val) when is_float(Val)   -> erlang:float_to_list(Val);
to_list(_)                        -> erlang:throw(badarg).

%% ============================================================================
%% Tests
%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_binary_test_() ->
	Test =
		[	{"atom",    a,       <<"a">>},
			{"string",  "s",     <<"s">>},
			{"binary",  <<"b">>, <<"b">>},
			{"integer", 1,       <<"1">>},
			{"float",   1.0,     <<"1.00000000000000000000e+00">>}],
	[{Desc, ?_assertEqual(Output, ?MODULE:to_binary(Input))} || {Desc, Input, Output} <- Test].

to_list_test_() ->
	Test =
		[	{"atom",    a,       "a"},
			{"string",  "s",     "s"},
			{"binary",  <<"b">>, "b"},
			{"integer", 1,       "1"},
			{"float",   1.0,     "1.00000000000000000000e+00"}],
	[{Desc, ?_assertEqual(Output, ?MODULE:to_list(Input))} || {Desc, Input, Output} <- Test].

-endif.

