% simple shop module
-module(shop).
-export([cost/1, total/1, totalV2/1, sum/1]).
-import(lists, [map/2]).

% get the cost
cost(orange)
    -> 5;
cost(newspaper)
    -> 7;
cost(apple)
    -> 2;
cost(pear)
    -> 9;
cost(milk)
    -> 8.

% get the total cost.
total([{What, Count}|T])
    -> cost(What) * Count + total(T);
total([])
    -> 0.

% test the sum.
sum(L)
    -> sum(L, 0).

% help for the sum function.
sum([H|T], N)
    -> sum(T, H + N);
sum([], N)
    -> N.

% total version 2
totalV2(L)
    -> lists:sum(map(fun({What, Count}) -> cost(What) * Count end, L)).
