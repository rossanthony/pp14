:- [maze].

% output the column numbers along the top row
header :-
	write('     '),
	header(1).
header(W) :-
	mazeSize(_, Width),
	W is Width + 1,
	nl.
header(Num) :-
	write(' '),
	write(Num),
	NextNum is Num + 1,
	header(NextNum).

divider :- 
	write('    +--'),
	divider(1).
divider(Width) :-
	mazeSize(_, Width),
	write('-+'), nl.
divider(N) :-
	write('--'),
	Next is N + 1,
	divider(Next).
 
row(H, _) :-
	mazeSize(Height, _),
	H is Height + 1.
row(H, Path) :-
	format("  ~w | ", [H]),
	rowItems(H, 1, Path), write('|'), nl,
	H2 is H + 1,
	row(H2, Path).

rowItems(_, Y, _) :-
	mazeSize(_, Width), Y is Width + 1.	
rowItems(X, Y, Path) :- 
	inPath([X, Y], Path), write('* '), YY is Y + 1, rowItems(X, YY, Path); 
	barrier(X, Y), write('x '), YY is Y + 1, rowItems(X, YY, Path);
	write('· '), YY is Y + 1, rowItems(X, YY, Path).

% check if a given set of Coords exists in the Path
inPath(Coords, [Coords|_]) :- !.
inPath(Coords, [_|Tail]) :- inPath(Coords, Tail).

printMaze(Path):-
   header,
   divider,
   row(1, Path),
   divider.
