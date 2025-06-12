last(XS, Z) :- append(_, [Z], XS).
first(XS, Z) :- append([Z], _ , XS).

reverso([],[]).
reverso([X|XS], R) :- reverso(XS, RS), append(RS, [X], R).

prefijo(P, L) :- append(P, _, L).

sufijo(S, L) :- append(_, S, L).


% Funciona como un sub-conjunto mas que como sub-lista.
sublista([], _).
sublista(S, L) :- 
    append(_, SP, L),
    append(S, _, SP),
    S \= [].

pertenece(X, L) :- append(_,[X|_] ,L).