Faster cube algorithm - possibility

Write a generalized function for checking how many pieces on the cube are next to a correct piece, regardless of positioning with respect to centers.

for any scrambled cube it should be able to return a single number that represents the solved-ness, in terms of number of correct connections.

have the computer test every possible set of next 3 moves which is 12 ^ 3 or 1,728 possible combinations. depending on calculation speed it could even go up to the
next 4 possible moves bringing it up to 12 ^ 4 or 20,736 calculations.

It will then select the set of 3 (or 4) moves that returned the highest level of solved-ness and perform them on the cube and then repeat. This will continue until
the cube is solved, or until we determine that this wont actually ever solve it. It will log every set of 3 (or 4) moves as it calculates them and i can follow 
with a real cube.

note - we may need to weight the value of different amounts of connected cubelets. for example if the are two chunks of 2 pieces, total 2 connections, that may count
for less than one chunk of 3 connected pieces which also totals 2 connections, or it may count for more, or that may depend on some other gauge of how far along in 
solving the cube it is.

note - I think it actually may be better to check the possible set of 1 next moves, 2 next moves, 3 next moves and possibly up to 4 next moves and for every
single possibility check the solved-ness. For example if it were given a cube that was 1 turn away from solved, we would not want to force it to make 4 next moves,
because when you are 1 move away there is NO POSSIBLE set of 4 moves that will solve it, however there is a set of 1 that will, so we want to check those as well.

so what it will do is check every possible turn of 1 face (the 6 sides clockwise and counter clockwise, resulting in 12 possibilities). It will check the solved-ness
at the end of each and log those. It will then check all 144 possible combinations of 2 face turns, check the solvedness of each and log those as well, eventually to be
compared with the results of the 1 turn combination results, and the 3 turn combination results (and possibly the 4 turn combination results as well). 
This would mean that it would actually make 12 + 144 + 1,728 = 1,884 calculations or 12 + 144 + 1,728 + 20,736 = 22,620 calculations per set of moves.

note - maybe we can slightly reduced the number of combinations (permutations really, because order matters) that the computer has to check by telling it to skip sets
of 2 moves, for example, where one move is a certain face clockwise, and the next one is that same face counterclockwise.