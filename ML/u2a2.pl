% a.
myLast([X],X).
myLast([H|T],LastItem) :-
	myLast(T,LastItem).



% b.
myMax([X],MaxItem) :-
	MaxItem >= X.
myMax([H|T],MaxItem) :-
	MaxItem >= H,
	myMax(T,MaxItem).



% c.
mySum([X],SumList) :-
	X is SumList.
mySum([H|T],SumList) :-
	mySum(T,SumList-H).



% d.
myOrdered(List) :-
	myOrderedUp(List);
	myOrderedDown(List).
myOrderedUp([_]).
myOrderedUp([H1|[H2|T]]) :-
	H1 =< H2,
	myOrderedUp([H2|T]).
myOrderedDown([_]).
myOrderedDown([H1|[H2|T]]) :-
	H1 >= H2,
	myOrderedDown([H2|T]).
