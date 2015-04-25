# Logic Programming with Prolog

Your task is to write a predicate

solve(From, To, Path)

which, given locations From and To, finds a Path going from From to To.
From and To are given as two element lists, and Path should be a list of
two-element lists. The first element of Path should be From, and the last
element should be To. Moves can be made horizontally or vertically, but
not diagonally.

For example,

solve([3,2], [2,6], [[3,2], [3,3], [2,3], [1,3], [1,4], [1,5], [1,6], [2,6]]).

## To test:

`swipl -s maze-solver.pl`

`solve([3,2], [2,6], Path).`