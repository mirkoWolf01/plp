estudiante(pepe).
estudiante(samuel).
estudiante(lionel).

notas([
    (pepe, biologia, 2),
    (samuel, biologia, 3),
    (samuel, lengua, 5),
    (pepe, lengua, 1),
    (pepe, biologia, 6),
    (pepe, lengua, 4),
    (pepe, analisis, 4),
    (lionel, analisis, 2),
    (lionel, lengua, 8)
    ]).

tieneMateriasAprobadas(E, M) :-
    notas(XS),
    member((E, M, N), XS),
    N >= 4. 

eliminarAplazos([], []).
eliminarAplazos([(E, M, N)|NS], L) :-
    N < 4, 
    tieneMateriasAprobadas(E, M),
    eliminarAplazos(NS, L).
eliminarAplazos([(E, M, N)|NS], [(E, M, N)|L]) :-
    N < 4, 
    not(tieneMateriasAprobadas(E, M)),
    eliminarAplazos(NS, L).
eliminarAplazos([(E, M, N)|NS], [(E, M, N)|L]) :-
    N >= 4,
    eliminarAplazos(NS, L).

notasDe(_, [], []).
notasDe(A, [(A, _, N)| NS], [N|L]) :-
    notasDe(A, NS, L).
notasDe(A, [(NA, _, _)| NS], L) :-
    A \= NA,
    notasDe(A, NS, L).

promedio(A, P) :-
    estudiante(A),
    notas(NS),
    eliminarAplazos(NS, NS_SA),
    notasDe(A, NS_SA, NotasDeA),
    sum_list(NotasDeA, SUMA), 
    length(NotasDeA, DIV),
    P is SUMA/DIV.
    
mejorEstudiante(A) :-
    estudiante(A),
    not(hayMejorPromedio(A)).

hayMejorPromedio(A) :-
    estudiante(X),
    X \= A,
    promedio(A, PA),
    promedio(X, PX), 
    PX > PA.

%%----------------------------------------------------------

generarCapicua([]).
generarCapicua([X]) :- natural(X).
generarCapicua(L) :-
    natural(I),
    append([I|LC], [I], L),
    generarCapicua(LC).

natural(1).
natural(X) :-
    var(X),
    natural(Y),
    X is Y + 1.
natural(X) :-
    nonvar(X),
    X > 1.


tokenizar(_, [], []).
tokenizar(D, F, [X|T]) :-
    member(X, D),
    append(X, FX, F),
    tokenizar(D, FX, T).

mayorCantPalabras(D, F, T) :-
    tokenizar(D, F, T),
    not(hayTokenMayor(D,F,T)).

hayTokenMayor(D,F, T) :-
    tokenizar(D, F, TP),
    T \= TP,
    length(TP, K),
    length(T, J),
    K > J.

%%----------------------------------------------------------

arbolGeneral(x).
arbolGeneral([]).
arbolGeneral([T|A]) :-
    arbolGeneral(T),
    arbolGeneral(A).
