%Nicht ganz wie Übung 9 (und 7.2), aber nah dran:
%Die Anzahl der expandierten Knoten wird nur am Ende ausgegeben 
%und die für jeden Zustand gespeicherten Kosten T sind nur die 
%tatsächlichen Kosten (die Heuristik wird beim Sortieren draufaddiert) 



heuristik(board(X1,X2,X3,X4,X5,X6,X7,X8,X9),board(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9),N):-
fehler(Y1,X1,0,R0),
fehler(Y2,X2,R0,R1),
fehler(Y3,X3,R1,R2),
fehler(Y4,X4,R2,R3),
fehler(Y5,X5,R3,R4),
fehler(Y6,X6,R4,R5),
fehler(Y7,X7,R5,R6),
fehler(Y8,X8,R6,R7),
fehler(Y9,X9,R7,R8), N is R8.

fehler(b,_,C,C):- !.
fehler(A,A,C,C):- !.
fehler(_,_,C,D):- D is C+1.

partition(_,_,[],[],[]):-!.
partition(Goal,[X,T],[[Y,TY]|Rest],[[Y,TY]|Small],Big):- 
                         heuristik(Goal,X,FX),heuristik(Goal,Y,FY),
                         T+FY < TY+FX,!, partition(Goal,[X,T],Rest,Small,Big).
partition(Goal,[X,T],[[Y,TY]|Rest],Small,[[Y,TY]|Big]):-
                         partition(Goal,[X,T],Rest,Small,Big).

qsort(_,[],[]):-!.
qsort(Goal,[[X,T]|L],LS):- partition(Goal,[X,T],L,Lsmall,Lbig),
                  qsort(Goal,Lsmall,S),
                  qsort(Goal,Lbig,B),
                  append(S,[[X,T]|B],LS).
kinderastar(X,T,L):-T1 is T+1,setof([Y,T1],move(0,X,Y),L),!.
kinderastar(_,_,[]).

astar(Goal,[[X,_]|_],Closed):-
	X == Goal,!,
	write('Found Solution for Goal '),write(X),nl,
	length(Closed,N),
	write('Size of ClosedList: '), write(N).
astar(Goal,[[X,T]|RestOpen],Closed):-member([X,T1],Closed),
                              T >= T1,!,astar(Goal,RestOpen,Closed).
astar(Goal,[[X,T]|RestOpen],Closed):-member([X,T1],Closed),
%Dieser Fall sollte bei einer konsistenten/monotonen Heuristik eigentlich nicht auftreten 
                              T < T1,!,kinderastar(X,T,L),
                              append(L,RestOpen,NewOpen),
                              qsort(Goal,NewOpen,Open),
                              astar(Goal,Open,Closed).
astar(Goal,[[X,T]|RestOpen],Closed):- write('expand:  '),write(X),nl,
                              heuristik(Goal,X,FX), C is T+FX,
                              write(' Total Cost is: '),write(C),nl,
                              write(' Actual Cost is: '),write(T),nl,
                              write(' Heuristik Cost is: '),write(FX),nl,
                              kinderastar(X,T,L),
                              append(L,RestOpen,NewOpen),
                              qsort(Goal,NewOpen,Open),
                              astar(Goal,Open,[[X,T]|Closed]). 

move(_,board(b,X2,X3,X4,X5,X6,X7,X8,X9),board(X2,b,X3,X4,X5,X6,X7,X8,X9)).
move(_,board(b,X2,X3,X4,X5,X6,X7,X8,X9),board(X4,X2,X3,b,X5,X6,X7,X8,X9)).
move(_,board(X1,b,X3,X4,X5,X6,X7,X8,X9),board(b,X1,X3,X4,X5,X6,X7,X8,X9)).
move(_,board(X1,b,X3,X4,X5,X6,X7,X8,X9),board(X1,X3,b,X4,X5,X6,X7,X8,X9)).
move(_,board(X1,b,X3,X4,X5,X6,X7,X8,X9),board(X1,X5,X3,X4,b,X6,X7,X8,X9)).
move(_,board(X1,X2,X3,X4,b,X6,X7,X8,X9),board(X1,b,X3,X4,X2,X6,X7,X8,X9)).

move(N,board(X1,X2,X3,X4,X5,X6,X7,X8,X9),board(Y7,Y4,Y1,Y8,Y5,Y2,Y9,Y6,Y3)):-

N1 is N+1,
N1 < 4,

move(N1,board(X3,X6,X9,X2,X5,X8,X1,X4,X7),board(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9)).

kindermoves(X,L):-setof(Y,move(0,X,Y),L),!.
kindermoves(_,[]).

sdepth(Goal,[X|_],Closed):-
	X==Goal,!,
	write('Found Solution for Goal '),write(X),nl,
	length(Closed,N),
	write('Size of ClosedList: '), write(N).
sdepth(Goal,[X|RestOpen],Closed):-
	member(X,Closed),!,
	sdepth(Goal,RestOpen,Closed).
sdepth(Goal,[X|RestOpen],Closed):-
	write('expand:  '),write(X),nl,	
	kindermoves(X,L),
	append(L,RestOpen,Open),
	sdepth(Goal,Open,[X|Closed]).

sbreadth(Goal,[X|_],Closed):-
	X==Goal,!,
	write('Found Solution for Goal '),write(X),nl,
	length(Closed,N),
	write('Size of ClosedList: '), write(N).
sbreadth(Goal,[X|RestOpen],Closed):-
	member(X,Closed),!,
	sbreadth(Goal,RestOpen,Closed).
sbreadth(Goal,[X|RestOpen],Closed):-
	write('expand:  '),write(X),nl,	
	kindermoves(X,L),
	append(RestOpen,L,Open),
	sbreadth(Goal,Open,[X|Closed]).
