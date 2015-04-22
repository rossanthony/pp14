#lang racket

printGrid([]).
printGrid(L) :- nl,
   divider,
   printBlock(L,L1),
   divider,
   printBlock(L1,L2),
   divider,
   printBlock(L2,_),
   divider.

printBlock(L,L3) :- 
   printRow(L,L1), nl,
   blankLine, 
   printRow(L1,L2), nl,
   blankLine, 
   printRow(L2,L3), nl.
 
printRow(L,L3) :-
   write('+ '),
   printTriplet(L,L1), write(' | '),
   printTriplet(L1,L2), write(' | '),
   printTriplet(L2,L3),
   write(' +').


blankLine :- 
   write('+         |         |         +'), nl.


% blankLine.

divider :-
   write('+---------+---------+---------+'), nl.

printTriplet(L,L3) :-
   printElement(L,L1), write('  '),
   printElement(L1,L2), write('  '),
   printElement(L2,L3).

printElement([X|L],L) :- var(X), !, write('.').
printElement([X|L],L) :- write(X).

:- printGrid([_,_,_,_,_,_,_,_,_,
        _,_,_,_,_,_,_,_,_, 
        _,_,_,_,_,_,_,_,_,
        _,_,_,_,_,_,_,_,_, 
        _,_,_,_,_,_,_,_,_, 
        _,_,_,_,_,_,_,_,_,
        _,_,_,_,_,_,_,_,_, 
        _,_,_,_,_,_,_,_,_, 
        _,_,_,_,_,_,_,_,_]).
