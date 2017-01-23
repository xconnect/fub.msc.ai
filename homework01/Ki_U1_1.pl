/* Stammbaum von genau drei Generationen aus der Serie "Die Legende von Korra" */
/* Laden der Datei ?- consult('/Users/username/Desktop/Test.pl'). */

/* Aufgabe 1) a) */

male(aang).
male(tenzin).
male(bumi).
male(meelo).

female(katara).
female(kya).
female(pema).
female(jinora).
female(ikki).

father(aang, kya).
father(aang, tenzin).
father(aang, bumi).
father(tenzin, jinora).
father(tenzin, ikki).
father(tenzin, meelo).

mother(katara, kya).
mother(katara, tenzin).
mother(katara, bumi).
mother(pema, jinora).
mother(pema, ikki).
mother(pema, meelo).

/* Aufgabe 1) b) */

parent(Elternteil, Kind) :- mother(Elternteil,Kind); father(Elternteil,Kind).

son(Kind, Elternteil) :- parent(Elternteil,Kind), male(Kind).

daugther(Kind, Elternteil) :- parent(Elternteil,Kind), female(Kind).

/* Falls es einen Vater gibt dann gib nur die anderen Kinder von dem Vater aus, sonst von der Mutter -> damit sind Duplikate ausgeschlossen */
/* Halbgeschwister sind nur dann moeglich, falls kein Vater vorhanden ist und die Kinder, die durch die Mutter gefunden werden einen anderen Vater haben */
brother(Kind_1, Kind_2) :- male(Kind_1), (father(Vater, Kind_1) -> father(Vater, Kind_2); (mother(Mutter, Kind_1), mother(Mutter, Kind_2))), (Kind_1 \= Kind_2).
sister(Kind_1, Kind_2) :- female(Kind_1), (father(Vater, Kind_1) -> father(Vater, Kind_2); (mother(Mutter, Kind_1), mother(Mutter, Kind_2))), (Kind_1 \= Kind_2).

grandfather(Grossvater, Enkelkind) :- male(Grossvater), parent(Grossvater, Kind), parent(Kind, Enkelkind).
grandmother(Grossmutter, Enkelkind) :- female(Grossmutter), parent(Grossmutter, Kind), parent(Kind, Enkelkind).

/* Aufgabe 1) c) */

/* Hier zaehlen aber keine Geschwister der Vorfahrend dazu -> lediglich nur die Eltern und die Eltern der Eltern uws. */
predecessor(Direkter_Vorfahre, Person) :- parent(Direkter_Vorfahre, Person).
predecessor(Indirekter_Vorfahre, Person) :- parent(Indirekter_Vorfahre, Direkter_Vorfahre), predecessor(Direkter_Vorfahre, Person).

/* Hier werden alle Kinder aufgelistet sowie die Kinder der Kinder usw. */
succsessor(Direkter_Nachfahre, Person) :- parent(Person, Direkter_Nachfahre).
succsessor(Indirekter_Nachfahre, Person) :- parent(Direkter_Nachfahre, Indirekter_Nachfahre), succsessor(Direkter_Nachfahre, Person).