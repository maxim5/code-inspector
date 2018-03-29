% Implementation of the game module.  See game.pl for a description of the
% exported predicates.
:- module(game, [cutoff/4, end_state/3, lookahead/1, move/3]).
:- use_module(util/misc, [count/3, replace_nth1/4]).

lookahead(6).

% move(+Player, +ListOfStates, ?[Move | ListOfStates]).
move(P, [S | Ss], [M, S | Ss]) :-
	transpose5(S, C1s),
	nth1(N, C1s, C1),
	player(C, P),
	move_column(C, C1, C2),
	replace_nth1(N, C2, C1s, C2s),
	transpose5(M, C2s).

% transpose5(+StateMatrix, -TransposedMatrix).
% True if TransposedMatrix it the transposed 5x5 StateMatrix.
transpose5(
	[ [E11, E12, E13, E14, E15]
	, [E21, E22, E23, E24, E25]
	, [E31, E32, E33, E34, E35]
	, [E41, E42, E43, E44, E45]
	, [E51, E52, E53, E54, E55]
	],
	[ [E11, E21, E31, E41, E51]
	, [E12, E22, E32, E42, E52]
	, [E13, E23, E33, E43, E53]
	, [E14, E24, E34, E44, E54]
	, [E15, E25, E35, E45, E55]
	]).

% move_column(+Player,+Column,-newColumn)
% performs a move in a column of the transposed matrix
move_column(P, [-, -, -, -, -], [-, -, -, -, P]).
move_column(P, [-, -, -, -, x], [-, -, -, P, x]).
move_column(P, [-, -, -, -, o], [-, -, -, P, o]).
move_column(P, [-, -, -, x, A], [-, -, P, x, A]).
move_column(P, [-, -, -, o, A], [-, -, P, o, A]).
move_column(P, [-, -, x, B, A], [-, P, x, B, A]).
move_column(P, [-, -, o, B, A], [-, P, o, B, A]).
move_column(P, [-, x, C, B, A], [P, x, C, B, A]).
move_column(P, [-, o, C, B, A], [P, o, C, B, A]).

% diagonals(+StateMatrix, -ListOfDiagonals).
% Returns a ListOfDiagonals in which four-in-a-row can occur.
diagonals(
	[ [E11, E12,   _, E14, E15]
	, [E21, E22, E23, E24, E25]
	, [  _, E32, E33, E34,   _]
	, [E41, E42, E43, E44, E45]
	, [E51, E52,   _, E54, E55]
	],
	[ [E12, E23, E34, E45]
	, [E14, E23, E32, E41]
	, [E11, E22, E33, E44, E55]
	, [E51, E42, E33, E24, E15]
	, [E21, E32, E43, E54]
	, [E52, E43, E34, E25]
	]).

cutoff(P, _, Ss, U) :-
	end_state(P, Ss, U).
cutoff(P, D, [S | _], U) :- D =< 0,
	O is -P,
	player(Cp, P),
	player(Co, O),
	lines(S, L),
	findall(X, (member(M, L), line4(M, X)), Xs),
	count_symbols(Cp, Xs, 0, X),
	count_symbols(Co, Xs, 0, Y),
	U is P * (X - Y).

end_state(_, [S | _], U) :-
	player(C, P), % checks both players using backtracking
	terminal(C, S),
	U is P * 666,
	!.
end_state(_, [S | _], 0) :-
	not((member(L, S), member('-', L))).

% terminal(+Player, +State).
terminal(P, S) :-
	lines(S, L),
	member([P, P, P, P, _], L), !.
terminal(P, S) :-
	lines(S, L),
	member([_, P, P, P, P], L), !.
terminal(P, S) :-
	lines(S, L),
	member([P, P, P, P], L).

% lines(+StateMatrix, ?ListOfRowsColumnsDiagonals).
lines(Rs, RCDs) :-
	transpose5(Rs, Cs),
	diagonals(Rs, Ds),
	append(Rs, Cs, RCs),
	append(RCs, Ds, RCDs).

line4([A, B, C, D], [A, B, C, D]).
line4([A, B, C, D, _], [A, B, C, D]).
line4([_, A, B, C, D], [A, B, C, D]).

count_symbols(_, [], N, N).
count_symbols(Cp, [L | Ls], N, M) :-
	player(Cp, P),
	O is -P,
	player(Co, O),
	member(Co, L), !,
	count_symbols(P, Ls, N, M).
count_symbols(Cp, [L | Ls], N, M) :-
	count(-, L, X),
	count(Cp, L, Y),
	Nn is N + X + 10 * Y,
	count_symbols(Cp, Ls, Nn, M).

% vim: filetype=prolog
