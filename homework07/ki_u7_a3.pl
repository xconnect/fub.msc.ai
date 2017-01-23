/**************************************************************************************/
/* KI - Zettel 7 - Nicolas Lehmann, Tobias Gleißer, Adrian Zubarev                    */
/**************************************************************************************/

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