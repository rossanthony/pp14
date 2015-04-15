mazeSize(5,9).
barrier(1, 8).
barrier(2, 1).
barrier(2, 2).
barrier(2, 4).
barrier(2, 5).
barrier(3, 4).
barrier(3, 7).
barrier(3, 9).
barrier(4, 4).
barrier(4, 7).
barrier(4, 8).
barrier(4, 9).
barrier(5, 2).


printInnerMaze(Path, [Row, Col]) :-
	write(Row) ,
	write(' | ') ,
	printElem(Path, [Row, Col]).

printElem(Path, [Row, Col]) :-
	Col =< 9, Row =< 5,
	member([Row, Col], Path), write('* '),
	NextCol is Col+1,!,printElem(Path, [Row, NextCol]).

printElem(Path, [Row, Col]) :-
	Col =< 9, Row =< 5,
	barrier(Row, Col), write('x '),
	NextCol is Col+1,!,printElem(Path, [Row, NextCol]).

printElem(Path, [Row, Col]) :-
	Col =< 9, Row =< 5, write('· '),
	NextCol is Col+1,!,printElem(Path, [Row, NextCol]).

printElem(Path, [Row, Col]) :-
	Row < 5, Col > 9,
	NewRow is Row + 1,
	write('|'), nl,!,printInnerMaze(Path, [NewRow, 1]).

printElem(_, [Row, Col]) :-
	Col > 9, Row =:= 5,
	write('|').	

printMaze(Path) :-
	nl, write('    1 2 3 4 5 6 7 8 9  '),
	nl, write('  +-------------------+'),
	nl, printInnerMaze(Path, [1, 1]),
	nl, write('  +-------------------+').

/* printMaze([[3,2], [3,3], [2,3], [1,3], [1,4], [1,5], [1,6], [2,6]]).	*/