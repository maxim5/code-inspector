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

-module(pt_kvstore).

-callback keys(Store) -> Vals
	when
		Store :: any(),
		Vals  :: list().

-callback values(Store) -> Keys
	when
		Store :: any(),
		Keys  :: list().

-callback get(Key, Store) -> Val
	when
		Key   :: any(),
		Val   :: any(),
		Store :: any().

-callback get(Key, Store, Default) -> Default | Val
	when
		Key     :: any(),
		Val     :: any(),
		Default :: any(),
		Store   :: any().

-callback get_in(Keys, Store) -> Val
	when
		Keys    :: list(),
		Val     :: any(),
		Store   :: any().

-callback get_in(Keys, Store, Default) -> Default | Val
	when
		Keys    :: list(),
		Val     :: any(),
		Default :: any(),
		Store   :: any().

-callback find(Key, Store) -> {ok, Val} | error
	when
		Key   :: any(),
		Val   :: any(),
		Store :: any().

-callback find_in(Keys, Store) -> {ok, Val} | error
	when
		Keys    :: list(),
		Val     :: any(),
		Store   :: any().

-callback with(Keys, Store) -> NewStore
	when
		Keys     :: list(),
		Store    :: any(),
		NewStore :: any().

-callback put(Key, Val, Store) -> NewStore
	when
		Key      :: any(),
		Val      :: any(),
		Store    :: any(),
		NewStore :: any().

-callback merge(Store1, Store2) -> NewStore
	when
		Store1   :: any(),
		Store2   :: any(),
		NewStore :: any().

-callback remove(Key, Store) -> NewStore
	when
		Key      :: any(),
		Store    :: any(),
		NewStore :: any().

-callback is_empty(Store) -> boolean()
	when
		Store    :: any().

