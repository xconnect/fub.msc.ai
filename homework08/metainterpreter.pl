/* Prolog simple meta interpreter - implements TRUE, AND, OR and SYSTEM CALLS */

/* prolog expressions to solve must be in brackets */

/***************************** examples *********************************/
/*                                                                      */
/*                                                                      */
/* ?- solve((vater(adam,theo))).                                        */
/* false.                                                               */
/*                                                                      */
/* ?- solve((vater(adam,abel))).                                        */
/* true.                                                                */
/*                                                                      */
/* ?- solve((vater(adam,abel),vater(adam,cain),vater(adam,theo))).      */
/* false.                                                               */
/*                                                                      */
/* solve((vater(adam,theo);(vater(adam,cain),vater(adam,abel)))).       */
/* true.                                                                */
/*                                                                      */
/************************************************************************/


solve(true) :- !.
solve((A,B)) :- A, solve(B).
solve((A;B)) :- A; solve(B).
solve(A) :- system(A), !, call(A).
solve(A) :- clause(A,B), solve(B).

system(=(_,_)).
system(==(_,_)).
system(fail).
system(nl).
system(read(_)).
system(write(_)).
system(is(_,_)).
system(>(_,_)).
system(<(_,_)).
system(clause(_,_)).
system(call(_)).
system(var(_)).

/* test facts and rules */

vater(adam,abel).
vater(adam,cain):-!.
vater(abel,isaac).
opa(X,Y):-!,vater(X,Z),vater(Z,Y).

a:-c.
a:- !,fail.
a:- b,!,c.
a:-c.
b.
c.