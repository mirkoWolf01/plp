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


borrar(LS, X, LR) :-
    append(L1, [X|L2], LS), !,
    append(L1, L2, LTEMP), 
    borrar(LTEMP, X, LR).
borrar(LS, _, LS).


sacarDuplicados([], []).
sacarDuplicados(L1, L2) :- 
    sacarDuplicadosAux(L1, [], L2).

sacarDuplicadosAux([], _ , []).
sacarDuplicadosAux([X|L1], HS, L2) :-
    member(X, HS),
    sacarDuplicadosAux(L1, HS, L2).
sacarDuplicadosAux([X|L1], HS, [X|L2]) :-
    not(member(X, HS)),
    sacarDuplicadosAux(L1, [X|HS], L2).

permutacion([], []).
permutacion(L1, [X|L2]) :-
    member(X, L1),
    borrarUno(L1, X, L1P),
    permutacion(L1P, L2).

borrarUno(LS, X, LRES) :-
    append(L1, [X|L2], LS),
    append(L1, L2, LRES).

reparto([], _, []).
reparto(L, N, [L1|LLISTAS]) :-
    length([L1|LLISTAS], N),
    append(L1, L2, L),
    M is N-1,
    reparto(L2, M, LLISTAS).


repartoSinVacias([], []).
repartoSinVacias(L, [L1|LLISTAS]) :-
    append(L1, L2, L),
    L1 \= [],
    repartoSinVacias(L2, LLISTAS).
    
