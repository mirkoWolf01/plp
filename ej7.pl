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
