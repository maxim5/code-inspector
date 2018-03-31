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

-module(pt_binary).

%% API
-export([
	join/1,
	join/2
]).

%% ============================================================================
%% API
%% ============================================================================

-spec join(binary() | [binary()]) -> binary().
join(L) ->
	join(L, <<>>).

-spec join(binary() | [binary()], binary()) -> binary().
join([H|T], Sep) ->
	lists:foldl(
		fun(Val, Acc) ->
			<<Acc/binary, Sep/binary, Val/binary>>
		end, H, T);
join([], _) -> <<>>;
join(L, _)  -> L.

%% ============================================================================
%% Tests
%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

join_test_() ->
	Sep = <<$,>>,
	Test =
		[ {"single value", [<<$b>>],            <<"b">>},
		  {"list empty",   [[]],                 <<>>},
		  {"list 1-val",   [[<<$a>>]],          <<$a>>},
		  {"list 2-val",   [[<<$a>>, <<$b>>]],  <<"ab">>},
			%% w/ separator
		  {"single value w/ separator", [<<$b>>,           Sep], <<"b">>},
		  {"list empty   w/ separator", [[],               Sep], <<>>},
		  {"list 1-val   w/ separator", [[<<$a>>],         Sep], <<$a>>},
		  {"list 2-val   w/ separator", [[<<$a>>, <<$b>>], Sep], <<"a,b">>} ],
	[{Desc, ?_assertEqual(Output, apply(?MODULE, join, Input))} || {Desc, Input, Output} <- Test].

-endif.

