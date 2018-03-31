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

-module(pt_modlist).

%% API
-export([
	calll/3,
	calll/4,
	callr/3,
	callr/4,
	calll_sublist/3,
	calll_sublist/4,
	callr_sublist/3,
	callr_sublist/4,
	find/3,
	sublistl/3,
	sublistr/3
]).

%% ============================================================================
%% API
%% ============================================================================

-spec calll(list(module()), atom(), list()) -> any().
calll(Mods, Fun, Args) ->
	Arity = length(Args),
	case find(Mods, Fun, Arity) of
		{ok, Mod} -> apply(Mod, Fun, Args);
		error     -> error({bad_call, Mods, Fun, Arity})
	end.

-spec calll(list(module()), atom(), list(), any()) -> any().
calll(Mods, Fun, Args, Default) ->
	Arity = length(Args),
	case find(Mods, Fun, Arity) of
		{ok, Mod} -> apply(Mod, Fun, Args);
		error     -> Default
	end.

-spec callr(list(module()), atom(), list()) -> any().
callr(Mods, Fun, Args) ->
	calll(lists:reverse(Mods), Fun, Args).

-spec callr(list(module()), atom(), list(), any()) -> any().
callr(Mods, Fun, Args, Default) ->
	calll(lists:reverse(Mods), Fun, Args, Default).

-spec calll_sublist(list(module()), atom(), list()) -> any().
calll_sublist(Mods, Fun, Args) ->
	Arity = length(Args) +1,
	case sublistl(Mods, Fun, Arity) of
		[Mod|_] = SubL -> apply(Mod, Fun, [SubL|Args]);
		[]             -> error({bad_call, Mods, Fun, Arity})
	end.

-spec calll_sublist(list(module()), atom(), list(), any()) -> any().
calll_sublist(Mods, Fun, Args, Default) ->
	Arity = length(Args) +1,
	case sublistl(Mods, Fun, Arity) of
		[Mod|_] = SubL -> apply(Mod, Fun, [SubL|Args]);
		[]             -> Default
	end.

-spec callr_sublist(list(module()), atom(), list()) -> any().
callr_sublist(Mods, Fun, Args) ->
	Arity = length(Args) +1,
	case sublistr(Mods, Fun, Arity) of
		[Mod|_] = SubL -> apply(Mod, Fun, [SubL|Args]);
		[]             -> error({bad_call, Mods, Fun, Arity})
	end.

-spec callr_sublist(list(module()), atom(), list(), any()) -> any().
callr_sublist(Mods, Fun, Args, Default) ->
	Arity = length(Args) +1,
	case sublistr(Mods, Fun, Arity) of
		[Mod|_] = SubL -> apply(Mod, Fun, [SubL|Args]);
		[]             -> Default
	end.

-spec find(list(module()), atom(), arity()) -> {ok, module()} | error.
find([H|T], Fun, Arity) ->
	case erlang:function_exported(H, Fun, Arity) of
		true  -> {ok, H};
		false -> find(T, Fun, Arity)
	end;
find([], _, _) ->
	error.

-spec sublistl(list(module()), atom(), arity()) -> list(module()).
sublistl([H|T] = Mods, Fun, Arity) ->
	case erlang:function_exported(H, Fun, Arity) of
		true  -> Mods;
		false -> sublistl(T, Fun, Arity)
	end;
sublistl([], _, _) ->
	[].

-spec sublistr(list(module()), atom(), arity()) -> list(module()).
sublistr(Mods, Fun, Arity) ->
	sublistr([], lists:reverse(Mods), Fun, Arity).

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec sublistr(list(module()), list(module()), atom(), arity()) -> list(module()).
sublistr(Acc, [H|T], Fun, Arity) ->
	Acc2 = [H|Acc],
	case erlang:function_exported(H, Fun, Arity) of
		true  -> Acc2;
		false -> sublistr(Acc2, T, Fun, Arity)
	end;
sublistr(_, [], _, _) ->
	[].

