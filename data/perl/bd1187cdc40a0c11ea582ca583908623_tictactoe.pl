% Implementation of the game module.  See game.pl for a description of the
% exported predicates.
:- module(game, [cutoff/4, end_state/3, lookahead/1, move/3]).

lookahead(4).

% move0(+Player_char, +State, -NewState, -LineList).
% -LineList not used here
move0(A,
	[ [-, B, C]
	, [D, E, F]
	, [G, H, I]
	],
    [ [A, B, C]
	, [D, E, F]
	, [G, H, I]
	],
    [ [A, B, C]
	, [A, D, G]
	, [A, E, I]
	]).
move0(B,[[A,-,C],[D,E,F],[G,H,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[A,B,C],[B,E,H]]).
move0(C,[[A,B,-],[D,E,F],[G,H,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[A,B,C],[C,F,I],[C,E,G]]).
move0(D,[[A,B,C],[-,E,F],[G,H,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[D,E,F],[A,D,G]]).
move0(E,[[A,B,C],[D,-,F],[G,H,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[D,E,F],[D,E,F],[A,E,I],[C,E,G]]).
move0(F,[[A,B,C],[D,E,-],[G,H,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[D,E,F],[C,F,I]]).
move0(G,[[A,B,C],[D,E,F],[-,H,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[G,H,I],[A,D,G],[C,E,G]]).
move0(H,[[A,B,C],[D,E,F],[G,-,I]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[G,H,I],[B,E,H]]).
move0(I,[[A,B,C],[D,E,F],[G,H,-]],
        [[A,B,C],[D,E,F],[G,H,I]],
        [[G,H,I],[C,F,I],[A,E,I]]).

terminal(W, [[W, W, W] | _]).
terminal(W, [_, [W, W, W] | _]).
terminal(W, [_, _, [W, W, W]]).
terminal(W,
	[ [W | _]
	, [W | _]
	, [W | _]
	]).
terminal(W,
	[ [_, W | _]
	, [_, W | _]
	, [_, W | _]
	]).
terminal(W,
	[ [_, _, W]
	, [_, _, W]
	, [_, _, W]
	]).
terminal(W,
	[ [W, _, _]
	, [_, W, _]
	, [_, _, W]
	]).
terminal(W,
	[ [_, _, W]
	, [_, W, _]
	, [W, _, _]
	]).

% two(+Player,+Drcs, -Line)
% finds all diags/rows/columns with at most 2 crosses
two(A, Drc, [A, A, B]) :- member([A, A, B], Drc).
two(A, Drc, [A, B, A]) :- member([A, B, A], Drc).
two(A, Drc, [B, A, A]) :- member([B, A, A], Drc).

% drc()
% generates all possible lines in which 3 crosses can occur
drc([ [A, B, C]
	, [D, E, F]
	, [G, H, I]
	],
	[ [A, B, C], [D, E, F], [G, H, I], [A, D, G]
	, [B, E, H], [C, F, I], [A, E, I], [C, E, G]
	]).

% return the number of lines/columns/diags that contain at most 2 crosses
count_two(A,S,N) :-
	drc(S,Drc),
	findall(X,two(A,Drc,X),L),
	length(L,N).

cutoff(Player, _Depth, States, Result) :-
	end_state(Player, States, Result).
cutoff(Player,Depth, [S|_R], N) :-
	Depth =< 0,
	player(Symbol,Player),
	opponent(OpSymbol,Player),
	count_two(Symbol,S,M1),
	count_two(OpSymbol,S,M2),
	N is (M1-M2) * Player.

	% smart evaluatuion strategies go here

end_state(_, [S | _], U) :-
	player(C, P), % checks both players using backtracking
	terminal(C, S),
	U is P * 666.
end_state(_, [S | _], 0) :-
	not((member(L, S), member('-', L))).

move(P, [S | Ss], [M, S | Ss]) :-
	player(C, P),
	move0(C, S, M, _).

% opponent(-OpponentSign, +Player)
% returns the sign of the player's opponent
opponent(o,Player) :-
	player(x,Player).
opponent(x,Player) :-
	player(o,Player).

% vim: filetype=prolog
