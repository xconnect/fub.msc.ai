
/**************************************************************************************/
/* KI - Zettel 8 - Nicolas Lehmann, Tobias Gleißer, Adrian Zubarev                    */
/**************************************************************************************/

/**************************************************************************************/
/* Aufgabe 2a)																		  */
/**************************************************************************************/

/* Beim 'quit' soll aufgehoert werden */
main :- greeting, repeat, write('> '), read(X), do(X), X == quit, !.

/* Hier haben wir eine Disjunktion innerhalb einer Konjuktion. ((Goal_1; Goal_2), Rest) */
prove(((Goal_1; Goal_2), Rest), Hist) :­ !, ((prov(Goal_1, [Goal_1 | Hist]), prove(Rest, Hist)); (!, prov(Goal_2, [Goal_2 | Hist]), prove(Rest, Hist))).

/* Hier haben wir ausschließlich eine Disjunktion. (Goal_1; Goal_2) */
prove((Goal_1; Goal_2), Hist) :­ !, ((prov(Goal_1, [Goal_1 | Hist])); (!, prov(Goal_2, [Goal_2 | Hist]))).