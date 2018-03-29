% Implementation of the game module.  See game.pl for a description of the
% exported predicates.
:- module(game, [cutoff/4, end_state/3, lookahead/1, move/3]).
:- use_module(util/misc, [foldl/4, xor/3]).

lookahead(8).

cutoff(P, _, Ss, U) :-
	end_state(P, Ss, U).
cutoff(P, D, [S | _], N) :- D =< 0,
	(
		nim_sum(S, 0)
	->
		N is P * 555
	;
		N is P * -555
	).

% nim_sum(+State, -Sum).
% See: http://en.wikipedia.org/wiki/Nim#Mathematical_theory
nim_sum(S, N) :-
	maplist(length, S, Ls),
	foldl(xor, 0, Ls, N).

% Unpack the last game state from the state sequence.
end_state(P, [S | _], U) :-
	terminal(P, S, U).

% Are all heaps empty?
terminal(P, [[]], U) :-
	U is P * 666.
terminal(P, [[] | T], U) :-
	terminal(P, T, U).

move(_, [S | Ss], [M, S | Ss]) :-
	subst_element(A, B, S, M),
	remove(A, B).

subst_element(E1, E2, [E1 | R], [E2 | R]).
subst_element(E1, E2, [Y | R], [Y | R1]) :-
	subst_element(E1, E2, R, R1).

% remove(?List, ?Suffix).
% True for the Suffix of the List.
remove([_ | R], R).
remove([_ | R], R1) :-
	remove(R, R1).

% vim: filetype=prolog
