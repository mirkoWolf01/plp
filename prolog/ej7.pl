interseccion([], _, []).
interseccion([X|L1], L2, [X|L3]) :-
    member(X, L2),
    interseccion(L1, L2, L3),
    not(member(X, L3)).
interseccion([X|L1], L2, L3) :-
    not(member(X, L2)),
    interseccion(L1, L2, L3).

partir(N, L, [], L) :- N =< 0.
partir(N, [X|L], [X|L1], L2) :- 
    M is N-1,
    partir(M, L, L1, L2).

borrar([], _, []).
borrar([Y|LO], X, [Y|LSX]) :- 
    Y \= X,
    borrar(LO, X, LSX).
borrar([X|LO], X, LSX) :- 
    borrar(LO, X, LSX).


/* sacarDuplicados([], []).
sacarDuplicados([X|L1], L2) :-
    member(X, L1),
    sacarDuplicados(L1, L2).
sacarDuplicados([X|L1], [X|L2]) :-
    not(member(X, L1)),
    sacarDuplicados(L1, L2). */

sacarDuplicados([], []).
sacarDuplicados(L1, L2) :- 
    sacarDuplicadosAux(L1, [], L2).

sacarDuplicadosAux([], _ , []).
sacarDuplicadosAux([X|L1], HS, L2) :-
    member(X, HS),
    sacarDuplicadosAux(L1, HS, L2).
sacarDuplicadosAux([X|L1], HS, [X|L2]) :-
    not(member(X, HS)),
    sacarDuplicadosAux(L1, [X|HS], L2). % Esta parte es lo realmente importante

/* insertar(X, [], [X]).
insertar(X, L, XL) :-
    append(A, B, L), 
    append(A, [X|B], XL). */

/* permutacion([], []).
permutacion(L1, [X|L2]) :-
    insertar(X, L, L2),
    permutacion(L1, LP).
     */

