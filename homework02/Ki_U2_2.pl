/* Laden der Datei (unter OS X) ?- consult('/Users/username/Desktop/Ki_U2_2.pl'). */

/* Aufgabe 2) a) */

myLast([Element], Element). /* Die Liste besteht nur noch aus einem Element, was zugleich das Letzte sein muesste. */
myLast([_|Restliste], Element) :- myLast(Restliste, Element).

/* Aufgabe 2) b) */

/* select(Elem, Liste1, Liste2) produziert in 'Liste2' immer eine neue Liste aus 'Liste1' nur ohne 'Elem' (alle Permutationen wo jeweils einmal 'Elem' in 'Liste1' geloescht wird) */
/* member(Elem, Liste) ueberprueft ob das 'Elem' in der 'Liste' vorkommt oder nicht */
/* \+( ) wird wahr wenn die ineere Definition sich als falsch erweist */
/* Das heißt wir produziern alle Liste ohne das maximale Element und ueberpruefen ob darin kein groesseres Element vorkommt */
/* Das Ermitteln des groessten Elements ist einfach, denn es werden einfach alle Kombinationen durchgelaufen bis die Definition wahr wird */

myMax(Liste, MaxElement) :- select(MaxElement, Liste, ListeOhneMaxElement), \+( (member(AnderesElement, ListeOhneMaxElement), AnderesElement > MaxElement) ).

/* Aufgabe 2) c) */

/* Wenn die Liste leer ist, ist die Summe gleich 0 */
/* Wenn die Liste aus einem Element besteht, dann ist es die Summe */
/* Wenn die Liste mehrere Elemente hat dann addiere die ersten zwei Elemente, baue eine neue Liste aus der Summe der beiden Elemente und der Resliste und pruefe rekursiv */

mySum([], 0).
mySum([Element], Summe) :- Summe is Element.
mySum([Element1|[Element2|Restliste]], Summe) :- mySum([Element1 + Element2|Restliste], Summe).

/* Aufgabe 2) d) */

/* Eine leere Liste ist als sortiert anzunehmen. */
/* Eine Liste aus einem Element ist als sortiert anzunehmen. Zugleich die Abbruchbedingung fuer eine rekursive Ueberpruefung ob eine Liste aufsteigend sortiert ist oder nicht. */
/* Wir nehmen an wir koennen nur pruefen ob die Liste aufsteigend sortiert ist, dann muss jedes Element <= dem nachfolger in der Liste sein (Loesung rekursiv) */

myOrdered([]) :- true.
myOrdered([_]) :- true.
myOrdered([Element1|[Element2|Restliste]]) :- Element1 =< Element2 -> myOrdered([Element2|Restliste]).