/**************************************************************************************/
/* KI - Zettel 7 - Nicolas Lehmann, Tobias Gleißer, Adrian Zubarev                    */
/**************************************************************************************/

/**************************************************************************************/
/* Aufgabe 1 - Wege in Graphen */
/**************************************************************************************/

/**************************************/
/* 1a - Graph Modellierung mit Prolog */
/**************************************/

/* con_base fungiert hier als Hilfsklasse, damit auch false zurückgegeben werden kann */

/* Graph vom Übungsblatt */
con_base(a,b).
con_base(a,c).
con_base(b,d).
con_base(b,e).
con_base(c,d).
con_base(c,e).
con_base(d,e).

/* Repräsentation für ungerichteten Graphen */
con( X, Y ) :- con_base( X, Y ).
con( X, Y ) :- con_base( Y, X ).


/********************************/
/* 1b - Finde Pfad von X nach Y */
/********************************/

/* Bilde den Pfad von der X Node zur Y Node */
path( X, Y, P ):-dfs( X, [X], Y, P ).

/* Wenn Solution und Path immer gleich sind (-> Unifikation), dann bekommt man die vollständige Lösung */
dfs( Goal, Path, Goal, Solution ) :- Solution = Path, !.
/* Bilde alle Pfade jeder PreNode zum Goal */
dfs( PreNode, Path, Goal, Solution ) :-
    con( PreNode, StartNode ),
    not( member( StartNode, Path ) ),
    dfs( StartNode, [StartNode|Path], Goal, Solution ).

/*************************/
/* 1c - Finde alle Pfade */
/*************************/

allPaths( X, Y, L ) :- findall( P, path( X, Y, P ), L).

/****************************************/
/* 1d - Pfad mit maximaler Knotenanzahl */
/****************************************/

maxPath( X, Y, M ) :-
    allPaths( X, Y, Paths ),
    maxList( Paths, M ).

/* Größere von zwei Listen */
maxOfTwoLists( X, Y, X ) :-
    length( X, LX ),
    length( Y, LY ),
    LX >= LY.
maxOfTwoLists( X, Y, Y ) :-
    length( X, LX ),
    length( Y, LY ),
    LX < LY.
 
/* Finde längste Liste */
maxList( [L], L ).
maxList( [ Head|Rest ], L ) :-
    maxList( Rest, Max ),
    maxOfTwoLists( Head, Max, L ).



/**************************************************************************************/
/* Aufgabe 2 - Quicksort */
/**************************************************************************************/

/* sort left list into right list. pivot element is the first element */
quicksort( [], [] ).
quicksort( [ X|Xs ], SortierteListe ) :-
    partition( Xs, X, Left, Right ),
    quicksort( Left, Ls ),
    quicksort( Right, Rs ),
    append( Ls, [ X|Rs ], SortierteListe ).

/* partitions a list X|Xs with pivot element Y into lists of X <= Y and X > Y */
/* if X <= Y put it in the left list ( head parameter [X|Ls] ), analogously X > Y */
partition( [], _, [], [] ).
partition( [ X|Xs ], Y, [ X|Ls ], Rs ) :-
    X =< Y,
    partition( Xs, Y, Ls, Rs ).
partition( [ X|Xs ], Y, Ls, [ X|Rs ] ) :-
    X > Y,
    partition( Xs, Y, Ls, Rs ).

/**************************************************************************************/
/* Aufgabe 3 - 8-Puzzle */
/**************************************************************************************/

/**************************************/
/* 3a - Graph Modellierung mit Prolog */
/**************************************/

/**************************************************************************************/
/* Wir verwenden nicht die board(X1,X2,X3,X4,X5,X6,X7,X8,X9) Darstellung, da diese    */
/* sonst die Regeln in ihrer Schreibweise nur lang machen wird. Ausserdem ist b == 0. */
/**************************************************************************************/

move([0,X2,X3, X4,X5,X6, X7,X8,X9], [X2,0,X3, X4,X5,X6, X7,X8,X9]).
move([0,X2,X3, X4,X5,X6, X7,X8,X9], [X4,X2,X3, 0,X5,X6, X7,X8,X9]).

move([X1,0,X3, X4,X5,X6, X7,X8,X9], [0,X1,X3, X4,X5,X6, X7,X8,X9]).
move([X1,0,X3, X4,X5,X6, X7,X8,X9], [X1,X3,0, X4,X5,X6, X7,X8,X9]).
move([X1,0,X3, X4,X5,X6, X7,X8,X9], [X1,X5,X3, X4,0,X6, X7,X8,X9]).

move([X1,X2,0, X4,X5,X6, X7,X8,X9], [X1,0,X2, X4,X5,X6, X7,X8,X9]).
move([X1,X2,0, X4,X5,X6, X7,X8,X9], [X1,X2,X6, X4,X5,0, X7,X8,X9]).

move([X1,X2,X3, 0,X5,X6, X7,X8,X9], [0,X2,X3, X1,X5,X6, X7,X8,X9]).
move([X1,X2,X3, 0,X5,X6, X7,X8,X9], [X1,X2,X3, X5,0,X6, X7,X8,X9]).
move([X1,X2,X3, 0,X5,X6, X7,X8,X9], [X1,X2,X3, X7,X5,X6, 0,X8,X9]).

move([X1,X2,X3, X4,0,X6, X7,X8,X9], [X1,X2,X3, 0,X4,X6, X7,X8,X9]).
move([X1,X2,X3, X4,0,X6, X7,X8,X9], [X1,X2,X3, X4,X6,0, X7,X8,X9]).
move([X1,X2,X3, X4,0,X6, X7,X8,X9], [X1,0,X3, X4,X2,X6, X7,X8,X9]).
move([X1,X2,X3, X4,0,X6, X7,X8,X9], [X1,X2,X3, X4,X8,X6, X7,0,X9]).

move([X1,X2,X3, X4,X5,0, X7,X8,X9], [X1,X2,0, X4,X5,X3, X7,X8,X9]).
move([X1,X2,X3, X4,X5,0, X7,X8,X9], [X1,X2,X3, X4,0,X5, X7,X8,X9]).
move([X1,X2,X3, X4,X5,0, X7,X8,X9], [X1,X2,X3, X4,X5,X9, X7,X8,0]).

move([X1,X2,X3, X4,X5,X6, 0,X8,X9], [X1,X2,X3, 0,X5,X6, X4,X8,X9]).
move([X1,X2,X3, X4,X5,X6, 0,X8,X9], [X1,X2,X3, X4,X5,X6, X8,0,X9]).

move([X1,X2,X3, X4,X5,X6, X7,0,X9], [X1,X2,X3, X4,X5,X6, 0,X7,X9]).
move([X1,X2,X3, X4,X5,X6, X7,0,X9], [X1,X2,X3, X4,X5,X6, X7,X9,0]).
move([X1,X2,X3, X4,X5,X6, X7,0,X9], [X1,X2,X3, X4,0,X6, X7,X5,X9]).

move([X1,X2,X3, X4,X5,X6, X7,X8,0], [X1,X2,X3, X4,X5,0, X7,X8,X6]).
move([X1,X2,X3, X4,X5,X6, X7,X8,0], [X1,X2,X3, X4,X5,X6, X7,0,X8]).

/**************************************************************************************/
/* Wir verwenden hier die board(X1,X2,X3,X4,X5,X6,X7,X8,X9) Darstellung.              */
/**************************************************************************************/

/* Idee: move(OldState, NewState) */

move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_X > 1, moveLeft([OS_X/OS_Y|OS_Y_Rest], NewState).
move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_X < 3, moveRight([OS_X/OS_Y|OS_Y_Rest], NewState).
move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_Y > 1, moveDown([OS_X/OS_Y|OS_Y_Rest], NewState).
move([OS_X/OS_Y|OS_Y_Rest], NewState) :- OS_Y < 3, moveUp([OS_X/OS_Y|OS_Y_Rest], NewState).

/* moveDir bewegt jetzt halt das Blank nach links, rechts, obn bzw. unten...
moveLeft ...
moveRight ...
moveUp ...
moveDown ...