% Implementation of the game module.  See game.pl for a description of the
% exported predicates.
:- module(game, [cutoff/4, end_state/3, lookahead/1, move/3]).
:- use_module(util/misc, [free_all/3, free_one/3, ground_all/2, replace_all/4]).

lookahead(2).

move(P, [S | Ss], [M, S | Ss]) :-
	player(C, P),
	free_all([C, -], S, B1),
	place_l(C, L),
	transform_l(L, B1),
	B1 \= S, % did we move the L?
	free_one([x], B1, B2),
	place_x(X),
	transform_x(X, B2),
	ground_all(-, B2),
	replace_all(y, x, B2, M),
	not(member(M, [S | Ss])).

% This board positions are possible
% this means an L can lie in this positions
% or in a transpostion (transpositions defined later!)
place_l(O,
	[ [O, O, O, _]
	, [O, _, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	]).
place_l(O,
	[ [_, O, O, O]
	, [_, O, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	]).
place_l(O,
	[ [_, _, _, _]
	, [O, O, O, _]
	, [O, _, _, _]
	, [_, _, _, _]
	]).
place_l(O,
	[ [_, _, _, _]
	, [_, O, O, O]
	, [_, O, _, _]
	, [_, _, _, _]
	]).
place_l(O,
	[ [_, _, _, _]
	, [_, _, _, _]
	, [O, O, O, _]
	, [O, _, _, _]
	]).
place_l(O,
	[ [_, _, _, _]
	, [_, _, _, _]
	, [_, O, O, O]
	, [_, O, _, _]
	]).

% transform_l(+Board, ?TransformedBoard).
% There are exactly eight transformations of each board.
transform_l(A, A).
transform_l(A, B) :-
	clockwise(A, B).
transform_l(A, B) :-
	clockwise(B, A).
transform_l(A, B) :-
	reflect_x(A, B).
transform_l(A, B) :-
	reflect_y(A, B).
transform_l(A, C) :-
	reflect_x(A, B),
	clockwise(B, C).
transform_l(A, C) :-
	reflect_x(A, B),
	clockwise(C, B).
transform_l(A, C) :-
	clockwise(A, B),
	clockwise(B, C).

% Do not use an x to prevent unification with the second x.
place_x(
	[ [y, _, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	]).
place_x(
	[ [_, y, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	]).
place_x(
	[ [_, _, _, _]
	, [y, _, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	]).
place_x(
	[ [_, _, _, _]
	, [_, y, _, _]
	, [_, _, _, _]
	, [_, _, _, _]
	]).

transform_x(A, A).
transform_x(A, B) :-
	clockwise(A, B).
transform_x(A, B) :-
	clockwise(B, A).
transform_x(A, C) :-
	clockwise(A, B),
	clockwise(B, C).

% clockwise(?Matrix1, ?Matrix1).
% True is Matrix2 is Matrix1 rotated clockwise by pi/2.
clockwise(
	[ [A11, A12, A13, A14]
	, [A21, A22, A23, A24]
	, [A31, A32, A33, A34]
	, [A41, A42, A43, A44]
	],
	[ [A41, A31, A21, A11]
	, [A42, A32, A22, A12]
	, [A43, A33, A23, A13]
	, [A44, A34, A24, A14]
	]).

reflect_x(A, B) :-
	reverse(A, B).

reflect_y(A, B) :-
	maplist(reverse, A, B).

cutoff(P, _, Ss, U) :-
	end_state(P, Ss, U), !.
cutoff(P, D, [S, S0 | _], U) :- D =< 0,
	no_border(P, S, R1),
	force_to_border(P, S, R2),
	old_is_blocked(P, S, S0, R3),
	in_gp(P, S, R4),
	x_is_moved(S, S0, R5),
	no_place_for_l(S, R6),
	U is (((R1+R2)+(R3+R4))+R5+R6).

no_place_for_l(S, U) :- 
	free_all([-], S,S1),
	place_l(a, L),
	free_all([-],L,L1),
	L1 \= S1,
	U is 100.
no_place_for_l(_,0).

% no_border(+P,+S) is true if the L of Player P is not placed at the border
no_border(P, S, U) :-
	player(C,P),
	opponent(Op, P),
	free_all([Op, -, x], S, B1),
	ground_all(-, B1),
	findall(X, border_positions(C,X), Xs),
	not(member(B1, Xs)),
	U is 50.
no_border(_, _, 0).

% old_is_blocked(+L1,+L2) is true if and only if the old board position
% L1 is blocked by a x in Board L2
old_is_blocked(P, S1, S0, U) :- 
	player(Sign, P),
	opponent(Sign2, P),
	free_all([Sign2, x, -], S0, B1),
	free_all([Sign, Sign2, -], S1, B2),
	B1 \= B2,
	U is 30.
old_is_blocked(_, _, _, 0).

all_border( _, []).
all_border( P, [ L | T ] ) :- 
	not(no_border(P,L)),
	all_border(P, T).

% force_to_border(+Player, +State)
% suceeds if we can make a move that forces the opponent to use a border state.
force_to_border(P, S, U) :-
	Op is P* (-1),
	findall(X,move(Op, S, X), Xs),
	all_border(Op,Xs),
	U is 100.
force_to_border(_, _, 0).

% in_gp(+Player, +State)
% succeeds if we are in a 'good' state.
in_gp(P, S, U) :-
	player(C,P),
	opponent(C2, P),
	gp(C, B1),
	transform_l(B1, B2),
	free_all([-], B2, B3),
	free_all([C,C2,-], S, S1),
	S1 = B3,
	U is 200.
in_gp(_, _, 0).

% x_is_moved(+State, +State, -Utility)
% if the x is moved on the board the Utility is xxx
x_is_moved(S1, S0, U) :- 
	free_all([e,o,-], S1, B1),
	free_all([e,o,-], S0, B2),
	ground_all(-, B1),
	ground_all(-, B2),
	B1 \= B2,
	U is 60.
x_is_moved(_, _, 0).

% end_state(+Player, +States, -Result)
% suceeds if we lose and returns result.
end_state(P, Ss, U) :-
	not(move(P, Ss, _)),
	U is P*(-666).

% theese are border positions (where our L lies at the boarder)
% use transform(+A,-B) to get all reflections and rotations.
border_position(P, [[P,P,P,-]
	 	   , [P,-,-,-]
		   , [-,-,-,-]
		   , [-,-,-,-]
		   ]).

border_position(P, [[P,P,P,-]
	 	   , [-,-,P,-]
		   , [-,-,-,-]
		   , [-,-,-,-]
		   ]).

border_positions(P, D) :- border_position(P,E), transform_l(E,D).

%gp = good position for us...
gp(P, [[P,-,-,-]
     , [P,P,P,-]
     , [-,-,x,-]
     , [-,-,-,-]
     ]).
gp(P, [[P,-,-,-]
     , [P,P,P,-]
     , [x,-,-,-]
     , [-,-,-,-]
     ]).
gp(P, [[P,-,-,-]
     , [P,P,P,-]
     , [-,-,-,-]
     , [-,-,-,-]
     ]).
gp(P, [[-,P,-,-]
     , [-,P,P,P]
     , [-,x,-,-]
     , [-,-,-,-]
     ]).
gp(P, [[-,P,-,-]
     , [-,P,P,P]
     , [-,-,-,x]
     , [-,-,-,-]
     ]).
gp(P, [[-,P,P,-]
     , [-,P,-,-]
     , [-,P,x,-]
     , [-,-,-,-]
     ]).
gp(P, [[x,-,-,-]
     , [-,P,P,-]
     , [-,P,-,-]
     , [-,P,-,-]
     ]).
gp(P, [[-,-,-,x]
     , [-,P,P,-]
     , [-,P,-,-]
     , [-,P,-,-]
     ]).

opponent(A, -1) :-player(A, 1).
opponent(A,  1) :-player(A, -1).

% vim: filetype=prolog
