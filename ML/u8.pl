%1) (ohne den system-Teil wäre auch gut gewesen)
solve(true) :- !.
solve((A,B)) :- !, solve(A), solve(B).
solve((A;B)) :- !, solve(A); solve(B).
solve(A) :- system(A), call(A).
solve(A) :- clause(A, B), solve(B).

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

%2)a) Das ist das komplette neue prove Prädikat. Die Änderung bei der Konjunktion ist notwendig, damit es Disjunktionen im linken Teil von Konjuntionen geben kann (Konjunktionen und Disjunktionen werden in prov nicht mehr abgefangen). Das ! ist notwendig, damit es keine Endlosschleife gibt. Bei der Disjunktion muss damit es funktioniert der Teil nach dem ! in Klammern stehen. 
prove(true,_) :- !.
prove((Goal,Rest),Hist) :- !,
	prove(Goal,Hist),
	prove(Rest,Hist).
prove((Goal;Rest),Hist) :- !,
	(prove(Goal,Hist);
	prove(Rest,Hist)).
prove(Goal,Hist) :-
	prov(Goal,[Goal|Hist]).

