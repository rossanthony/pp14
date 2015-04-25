:- ensure_loaded(['maze-printer.pl']).

% Testing the maze printer...
%
% printMaze([[3,2], [3,3], [2,3], [1,3], [1,4], [1,5], [1,6], [2,6]]).	

allowedMove(Ypos, Xpos, Path) :-
	mazeSize(YposMax, XposMax),
	Ypos > 0, Xpos > 0,
	Ypos =< YposMax, Xpos =< XposMax,
	\+ (member([Ypos,Xpos], Path)),
	\+ (barrier(Ypos,Xpos)).

move([Ypos, OldXpos], [Ypos, NewXpos], Path) :-
	NewXpos is OldXpos + 1,
	write('Ypos:'), write(Ypos), nl, 
	write('OldXpos:'), write(OldXpos), nl, 
	write('NewXpos:'), write(NewXpos), nl, 
	allowedMove(Ypos,NewXpos, Path).

move([OldYpos,Xpos], [NewYpos,Xpos], Path) :-
	NewYpos is OldYpos + 1,
	allowedMove(NewYpos,Xpos, Path).

move([OldYpos,Xpos], [NewYpos,Xpos], Path) :-
	NewYpos is OldYpos - 1,
	allowedMove(NewYpos,Xpos, Path).

move([Ypos,OldXpos], [Ypos,NewXpos], Path) :-
	NewXpos is OldXpos - 1,
	allowedMove(Ypos,NewXpos, Path).

exploreMaze(From, To, Path, Path) :-
	write('From: '), write(From), nl, 
	write('To: '), write(To), nl, 
	write('Path: '), write(Path), nl, 
	From == To.
exploreMaze(From, To, Path, OldPath) :-
	write('-----'), nl, 
	move(From, NextStep, Path),
	write('From: '), write(From), nl, 
	write('NextStep: '), write(NextStep), nl, 
	write('OldPath: '), write(OldPath), nl, 
	write('[NextStep|Path]: '), write([NextStep|Path]), nl, 
	write('-----'), nl, 
	exploreMaze(NextStep, To, [NextStep|Path], OldPath).

solve(From, To, Path) :-
	exploreMaze(From, To, [From], Result),
	printMaze(Result).

% Pretty efficient at solving paths going from left of the maze to right...
solve([3,2], [5,3], Path).
% But inefficient when going from right to left...
solve([5,5], [3,2], Path).
% @TODO look at enhancing the move predicate so it has less bias for going from left to right
