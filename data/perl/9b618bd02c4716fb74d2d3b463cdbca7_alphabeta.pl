% Implementation of the strategy module.  See strategy.pl for a description of
% the exported predicates.
:- module(strategy, [play/4]).
:- use_module(util/misc, [keymax/3, keymin/3]).

% See also: % http://en.wikipedia.org/wiki/Alpha-beta_pruning

% play(+Player, +Depth, +ListOfStates, -Utility-Move).
% Will compute the optimal Utility-Move pair.
play(1, D, Ss, U-M) :-
	max(D, -999-[], 999-[], Ss, U-M).
play(-1, D, Ss, U-M) :-
	min(D, -999-[], 999-[], Ss, U-M).

% max(+Depth, +Alpha, +Beta, +ListOfStates, -Utility-Move).
max(D, _, _, Ss, U-none) :-
	cutoff(1, D, Ss, U), !.
max(D, A, B, Ss, U-M) :-
	findall(X, move(1, Ss, X), Xs),
	Dmm is D - 1,
	max_child(Dmm, A, B, -999-[], Xs, U-[M | _]).

% max_child(+Depth, +Alpha, +Beta, +ZeroElement, +ListOfListOfStates,
% -Utility-ListOfStates).
% Computes the child with the maximal utility.  Basically a foldl(keymax, ...).
max_child(_, _, B, Z, _, Z) :- keymax(B, Z, Z), !.
max_child(_, _, _, Z, [], Z).
max_child(D, A, B, X, [C | Cs], M) :-
	min(D, A, B, C, Y-_),
	keymax(X, Y-C, Z),
	keymax(Z, A, An),
	max_child(D, An, B, Z, Cs, M).

% Analogous to max/5.
min(D, _, _, Ss, U-none) :-
	cutoff(-1, D, Ss, U), !.
min(D, A, B, Ss, U-M) :-
	findall(X, move(-1, Ss, X), Xs),
	Dmm is D - 1,
	min_child(Dmm, A, B, 999-[], Xs, U-[M | _]).

% Analogous to max_child/6.
min_child(_, A, _, Z, _, Z) :- keymin(A, Z, Z), !.
min_child(_, _, _, Z, [], Z).
min_child(D, A, B, X, [C | Cs], M) :-
	max(D, A, B, C, Y-_),
	keymin(X, Y-C, Z),
	keymin(Z, B, Bn),
	min_child(D, A, Bn, Z, Cs, M).

% vim: filetype=prolog
