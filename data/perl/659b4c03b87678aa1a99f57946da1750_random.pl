% Implementation of the strategy module.  See strategy.pl for a description of
% the exported predicates.
:- module(strategy, [play/5]).

% A simplification of Helmar Gust's dumb.pl.
% Just selects a random move from all possible next moves.

play(Player, Depth, State, none, Utility) :-
	cutoff(Player, Depth, State, Utility), !.
play(Player, _, State, Move, 0) :-
	findall(X, move(Player, State, X), Ms),
	length(Ms, L),
	N is random(L),
	nth0(N, Ms, Move).

% vim: filetype=prolog
