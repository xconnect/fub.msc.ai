%1)

edge(a, b).
edge(a, c).
edge(b, d).
edge(b, e).
edge(c, d).
edge(c, e).
edge(d, e).

con(A, B) :- edge(A, B); edge(B, A).

path(X, Y, P) :- path(X, Y, [X], P).
path(X, X, _, [X]).
path(X, Y, V, [X | P]) :-
     con(X, Z),
     not(member(Z, V)),
     path(Z, Y, [Z | V], P).

allPaths(X, Y, L) :- setof(P, path(X, Y, P), L).

maxPath(X, Y, M) :- allPaths(X, Y, L), longest(L, M).

longest(L, V) :-
        select(V, L, R),
        \+((member(X, R), length(X, L1), length(V, L2), L1 > L2)).

%so ginge longest mit rekursion:
maxL([H],H).
maxL([H|T],H) :- maxL(T,X), length(H,HL), length(X,XL), HL>= XL.
maxL([H|T],X) :- maxL(T,X), length(H,HL), length(X,XL), HL < XL.


%2)

quicksort([],[]).
quicksort([a],[a]).
quicksort([X|T], Sorted) :-
  partition(T, X, L, R),
        quicksort(L, LS),
  quicksort(R, RS),
        append(LS, [X|RS], Sorted).

partition([],_,_,_).
partition([X|T], A, [X|L], R) :- X =< A, partition(T, A, L, R).
partition([X|T], A, L, [X|R]) :- X > A, partition(T, A, L, R).

%(es gibt auch schon partition/4 und partition/5 fertig in SWI prolog)

%3)a)

move([b,X2,X3, X4,X5,X6, X7,X8,X9], [X2,b,X3, X4,X5,X6, X7,X8,X9]).
move([b,X2,X3, X4,X5,X6, X7,X8,X9], [X4,X2,X3, b,X5,X6, X7,X8,X9]).

move([X1,b,X3, X4,X5,X6, X7,X8,X9], [b,X1,X3, X4,X5,X6, X7,X8,X9]).
move([X1,b,X3, X4,X5,X6, X7,X8,X9], [X1,X3,b, X4,X5,X6, X7,X8,X9]).
move([X1,b,X3, X4,X5,X6, X7,X8,X9], [X1,X5,X3, X4,b,X6, X7,X8,X9]).

move([X1,X2,b, X4,X5,X6, X7,X8,X9], [X1,b,X2, X4,X5,X6, X7,X8,X9]).
move([X1,X2,b, X4,X5,X6, X7,X8,X9], [X1,X2,X6, X4,X5,b, X7,X8,X9]).

move([X1,X2,X3, b,X5,X6, X7,X8,X9], [b,X2,X3, X1,X5,X6, X7,X8,X9]).
move([X1,X2,X3, b,X5,X6, X7,X8,X9], [X1,X2,X3, X5,b,X6, X7,X8,X9]).
move([X1,X2,X3, b,X5,X6, X7,X8,X9], [X1,X2,X3, X7,X5,X6, b,X8,X9]).

move([X1,X2,X3, X4,b,X6, X7,X8,X9], [X1,X2,X3, b,X4,X6, X7,X8,X9]).
move([X1,X2,X3, X4,b,X6, X7,X8,X9], [X1,X2,X3, X4,X6,b, X7,X8,X9]).
move([X1,X2,X3, X4,b,X6, X7,X8,X9], [X1,b,X3, X4,X2,X6, X7,X8,X9]).
move([X1,X2,X3, X4,b,X6, X7,X8,X9], [X1,X2,X3, X4,X8,X6, X7,b,X9]).

move([X1,X2,X3, X4,X5,b, X7,X8,X9], [X1,X2,b, X4,X5,X3, X7,X8,X9]).
move([X1,X2,X3, X4,X5,b, X7,X8,X9], [X1,X2,X3, X4,b,X5, X7,X8,X9]).
move([X1,X2,X3, X4,X5,b, X7,X8,X9], [X1,X2,X3, X4,X5,X9, X7,X8,b]).

move([X1,X2,X3, X4,X5,X6, b,X8,X9], [X1,X2,X3, b,X5,X6, X4,X8,X9]).
move([X1,X2,X3, X4,X5,X6, b,X8,X9], [X1,X2,X3, X4,X5,X6, X8,b,X9]).

move([X1,X2,X3, X4,X5,X6, X7,b,X9], [X1,X2,X3, X4,X5,X6, b,X7,X9]).
move([X1,X2,X3, X4,X5,X6, X7,b,X9], [X1,X2,X3, X4,X5,X6, X7,X9,b]).
move([X1,X2,X3, X4,X5,X6, X7,b,X9], [X1,X2,X3, X4,b,X6, X7,X5,X9]).

move([X1,X2,X3, X4,X5,X6, X7,X8,b], [X1,X2,X3, X4,X5,b, X7,X8,X6]).
move([X1,X2,X3, X4,X5,X6, X7,X8,b], [X1,X2,X3, X4,X5,X6, X7,b,X8]).


%3)b)

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


%oder auch so:

move2(board(X1,X2,X3,X4,X5,X6,X7,X8,X9), board(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9))
 :- L1 = [X1,X2,X3,X4,X5,X6,X7,X8,X9], 
    L2 = [Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9],
    nth0(I1, L1, b, R1), nth0(I2, L1, M),
    nth0(I2, L2, b), nth0(I1, L2, M, R2),
    select(M, R1, b, R2),
    specialRule(I1,I2).

specialRuleH(I,V) :- I // 3 =:= V // 3, I =:= (V+1).
specialRuleH(I,V) :- I // 3 =:= V // 3, I+1 =:= V.
specialRule(I,V) :- specialRuleH(I,V).
specialRule(I,V) :- specialRuleH(I // 3, V // 3), mod(I,3) =:= mod(V,3).


%oder so:

move3(A, B) :- movex(A, B, 0, true).

% Bewegung der Lücke nach links
movex(board(A, B, C, D, b, F, G, H, I), board(A, B, C, b, D, F, G, H, I), S, _) :- (S // 3) =\= 1.

% Bewegung der Lücke nach rechts
movex(board(A, B, C, D, b, F, G, H, I), board(A, B, C, D, F, b, G, H, I), S, _) :- S < 6.

% Bewegung der Lücke nach oben
movex(board(A, B, C, D, b, F, G, H, I), board(A, b, C, D, B, F, G, H, I), S, _) :- (S mod 3) =\= 1.

% Bewegung der Lücke nach unten
movex(board(A, B, C, D, b, F, G, H, I), board(A, B, C, D, H, F, G, b, I), S, _) :- (S mod 3) =\= 2.

% Bewegen des Quadrats nach unten, überstehende Felder kommen oben wieder dran
movex(board(A, B, C, D, E, F, G, H, I), board(J, K, L, M, N, O, P, Q, R), S, T) :- (S mod 3) =\= 2, T,
	movex(board(G, H, I, A, B, C, D, E, F), board(P, Q, R, J, K, L, M, N, O), S + 1, true).

% Bewegen des Quadrats nach rechts, überstehende Felder kommen links wieder dran
movex(board(A, B, C, D, E, F, G, H, I), board(J, K, L, M, N, O, P, Q, R), S, _) :- S < 6,
	movex(board(C, A, B, F, D, E, I, G, H), board(L, J, K, O, M, N, R, P, Q), S + 3, false).

% S ist eine Zahl die bestimmt, wo sich die Ränder des Quadrates befinden und welche Bewegungen somit erlaubt sind

% T ist ein Boolean, das aussagt, ob noch eine Bewegung nach unten erlaubt ist, um zu verhindern, dass mehrere
% Kombinationen das gleiche Ergebnis haben und sich das Ergenis so wiederholen würde
