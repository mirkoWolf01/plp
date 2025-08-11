desde(X,X).
desde(X,Y) :- 
    N is X+1, 
    desde(N,Y).


/* Tiene que ser (+X, -Y) para que funcione. Sino se cuelga. */

desdeReversible(X,Y) :- 
    var(Y), 
    desde(X, Y).
desdeReversible(X,Y) :- 
    nonvar(Y),
    X =< Y.
