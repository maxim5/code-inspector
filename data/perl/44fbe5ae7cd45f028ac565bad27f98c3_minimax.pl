% Implementation of the strategy module.  See strategy.pl for a description of
% the exported predicates.
:- module(strategy, [play/5]).
:- use_module(util/misc, [foldl/4, keymax/3]).

% http://en.wikipedia.org/wiki/Minimax
%
% function minimax(node, depth)
%    if node is a terminal node or depth = 0
%        return the heuristic value of node
%    else
%        let ? := -?
%        foreach child of node
%            let ? := max(?, -minimax(child, depth-1))
%        return ?

play(P, D, [N | Ns], [M, N | Ns], U) :-
	minimax(P, D, N, U-M).

% minimax(+Player, +Depth, +Node, -Utility-Move).
% Determines the next Move and its Utility from the parent Node.
% The sign of a leaf node is determined by its recursion depth.
% TODO: Really?  Check this!
minimax(P, D, N, 1-none) :-
	cutoff(P, D, [N], _),
	!.
minimax(P, D, N, C) :-
	findall(X, move(P, [N], [X | _]), Cs),
	O is -P,
	Dmm is D - 1,
	best_child(O, Dmm, (-999)-[], Cs, C).

% best_child(+Player, +Depth, +CurrentMaximum, +ListOfNodes, -Maximum).
% Computes the Maximum of ListOfNodes and the CurrentMaximum in a foldl like
% manner.
% NOTE: min(a, b) = -max(-a, -b).
best_child(_, _, M, [], M).
best_child(P, D, X, [C | Cs], M) :-
	minimax(P, D, C, Y-_),
	Yn is -Y,
	keymax(X, Yn-C, Z),
	best_child(P, D, Z, Cs, M).

% vim: filetype=prolog
