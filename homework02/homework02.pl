% Aufgabe 1a) [1 Punkt]

% Aufgabe 1b) [2 Punkte]

% Aufgabe 1c) [3 Punkte]

% Aufgabe 2a) [1 Punkt]
% myLast([H|[]] , LastItem) :- (H==LastItem).
% myLast([H|R] , LastItem) :- myLast(R,LastItem).

% Aufgabe 2b) [1 Punkt]
% myMax([H|[]] , MaxItem) :- H == MaxItem
% myMax([H|T] , MaxItem) :- H1 <= MaxItem, myMax(T, MaxItem)

% Aufgabe 2c) [1 Punkt]
% mySum(ListOfNumbers, SumList) :- mySumHelp(ListOfNumbers, SumList, 0).
% mySumHelp([],SumList,Akk) :- (SumList is Akk).
% mySumHelp([H|T],SumList,Akk) :- mySumHelp(T,SumList,T+Akk).

% Aufgabe 2d) [1 Punkt]
% myOrdered([H|[]]) :- True
% myOrdered([H1,H2|L]) :- H1 =< H2 , myOrdered([H2|L]).
% myOrdered(ListOfNumbers) :- 