/* A. Zubarev, T. Gleissner, N. Lehmann */

/* Aufgabe 1a) */
kinder(X,L) :- setof(Y, move(X,Y), L), !.
kinder(_,[]).


/**********************************************************************************************************/
/* ACHTUNG: Bei den nachfolgenden Teilaufgaben war es nicht ganz klar ob der Start- sowie der Zielzustand */
/* mitgezählt werden sollten oder nicht. Falls der Startzustand nicht zur Lösung gehört, muss man nur bei */
/* breitensuche und tiefensuche mit -1 statt 0 beginnen. Ist der Zielzustand nicht gewollt, dann muss man */
/* in den ersten Regeln von bfs und dfs incr(Number, N) am Ende entfernen und dabei Number und N durch _  */
/* ersetzen. Sind beide Zustände unwichtig, so müssen beide Modifikationen durchgeführt werden. 		  */
/**********************************************************************************************************/

/* Aufgabe 1b) */
incr(X, X1) :- X1 is X+1.
breitensuche(S,Z,N) :- bfs([Z],[S],[],0,N).
bfs(Goals, [X|_], _, Number, N) :- member(X, Goals), !, write(X), nl, incr(Number, N). 
bfs(Goals, [X|RestOpen], Closed, Number, N) :- member(X, Closed), !, bfs(Goals, RestOpen, Closed, Number, N). 
bfs(Goals, [X|RestOpen], Closed, Number, N) :- write(X), nl, kinder(X, L), append(RestOpen, L, Open), incr(Number, NewNumber), bfs(Goals, Open, [X|Closed], NewNumber, N).

/* Aufgabe 1c) */
tiefensuche(S,Z,N) :- dfs([Z],[S],[],0,N).
dfs(Goals, [X|_], _, Number, N) :- member(X, Goals), !, write(X), nl, incr(Number, N).
dfs(Goals, [X|RestOpen], Closed, Number, N) :- member(X, Closed), !, dfs(Goals, RestOpen, Closed, Number, N). 
dfs(Goals, [X|RestOpen], Closed, Number, N) :- write(X), nl, kinder(X, L), append(L, RestOpen, Open), incr(Number, NewNumber), dfs(Goals, Open, [X|Closed], NewNumber, N).


/* Aufgabe 2a) */
heuristik(X,Z,N) :- X = board(X1,X2,X3, X4,X5,X6, X7,X8,X9), Z = board(Y1,Y2,Y3, Y4,Y5,Y6, Y7,Y8,Y9), heuristik_help([X1,X2,X3, X4,X5,X6, X7,X8,X9], [Y1,Y2,Y3, Y4,Y5,Y6, Y7,Y8,Y9], 0, N).
heuristik_help([],[],Number,N) :- N is Number.
heuristik_help([X|Rest],[Y|OtherRest],Number,N) :- (X \= Y, X \= 0, Y \= 0, incr(Number, NewNumber)) -> heuristik_help(Rest,OtherRest,NewNumber,N);heuristik_help(Rest,OtherRest,Number,N).


/* Aufgabe 2b) */
kinder(X, T, Z, L) :- findall([Zustand, Kosten],(move(X, Zustand), heuristik(Zustand, Z, N), Kosten is T + N), L).

/* Aufgabe 2c) */
astern(S, Z, N) :- heuristik(S,Z,N), stern(Z,[[S,N]],[], 0, N), !.
stern(Goal, [[X,T] | _], _, Number, N) :- member(X, Goal), !, write('['), write(X), write('|'), write(T), write(']'), nl, incr(Number, N), !.
stern(Goal, [[X,T] | RestOpen], Closed, Number, N) :- member([X,T1], Closed), T >= T1, !, stern(Goal, RestOpen, Closed, Number, N).
stern(Goal, [[X,T] | RestOpen], Closed, Number, N) :- member([X,T1], Closed), T < T1, !, kinder(X,T,Goal,L), append(L, RestOpen, NewOpen), quicksort(NewOpen, Open), stern(Goal, Open, [[X,T]|Closed], Number, N).
stern(Goal, [[X,T] | RestOpen], Closed, Number, N) :- write('['), write(X), write('|'), write(T), write(']'), nl, heuristik(X,Goal,T), kinder(X,T,Goal,L), append(L, RestOpen, NewOpen), quicksort(NewOpen, Open), incr(Number, NewNumber), stern(Goal,Open,[[X|T],Closed], NewNumber, N).

quicksort( [], [] ).
quicksort( [ [X,P]|Xs ], SortierteListe ) :- partition( Xs, P, Left, Right ), quicksort( Left, Ls ), quicksort( Right, Rs ), append( Ls, [ [X,P]|Rs ], SortierteListe ).

partition( [], _, [], [] ).
partition( [ [Board,T1]|Rest ], P, [ [Board,T1]|LsRest ], Rs ) :- T1 =< P, partition( Rest, P, LsRest, Rs ).
partition( [ [Board,T1]|Rest ], P, Ls, [ [Board,T1]|RsRest ] ) :- T1 > P, partition( Rest, P, Ls, RsRest ).