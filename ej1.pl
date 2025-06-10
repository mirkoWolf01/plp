padre(juan, carlos).
padre(juan, luis).

padre(carlos, daniel).
padre(carlos, diego).

padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

/* ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).
 */

ancestro(X, X).
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).

hijo(X,Y) :- padre(Y,X).
hermano(X,Y) :- padre(Z,X), padre(Z, Y), X \= Y.


descendiente(X, Z) :- padre(Z, X).
descendiente(X, Y) :- padre(Y, Z), descendiente(X, Z).


/* 
I. El resultado de abuelo(X, manuel), es X = juan


IV. Habria que consultar abuelo(juan, X).
V. Habria que consulrar hermano(pablo, X).
 
VII. El predicado ancestro(juan, X) devuelve todos los X a los que juan es su ancestro. Es decir, que X es descendiente de juan. 
Si pedis mas de un resultado, llega un momento en el que el programa se cuelga porque para resolver ancestro(X, Y) llama a ancestro (Z, X), lo cual a su vez llama a otra cosa, es decir, nunca termina de evaluar.

VIII. Mi solucion es invertir el orden de las condiciones, tal que primero se chequee algun Z padre de X y luego busque para ese Z definido un Y que cumpla que Z es su ancestro. Asi hasta que te queda ancestro(Z,Z).
 */