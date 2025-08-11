vacio(nil).

raiz(bin(_, V, _), V).

altura(nil, 0).
altura(bin(L, _, R), H) :-
    altura(L, HL),
    altura(R, HR),
    H is max(HL, HR) + 1.

cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(L, _, R), C) :-
    cantidadDeNodos(L, LC),
    cantidadDeNodos(R, RC),
    C is LC + RC + 1.

inorder(nil, []).
inorder(bin(L, N, R), LS) :-
    inorder(L, LLS), 
    inorder(R, RLS),
    append(LLS, [N|RLS], LS).

/* 
A1 = bin(nil, 1, nil).
A3 = bin(bin(nil, 2, nil), 1, bin(nil, 3, nil)).    
A5 = bin(bin(bin(nil, 4, nil), 2, nil), 1, bin(nil, 3, bin(nil, 5, nil))). */

arbolConInorder([], nil).
arbolConInorder(LS, bin(L, E, R)) :-
    append(LLS, [E|RLS], LS),
    arbolConInorder(LLS, L), 
    arbolConInorder(RLS, R).
    

esABB(nil).
esABB(bin(L, N, R)) :-
    abbAUX(L, N, left),
    abbAUX(R, N, right), !.

abbAUX(nil, _, _).
abbAUX(bin(L, V, R), P, left) :-
    V < P, 
    abbAUX(L, V, left),
    abbAUX(R, V, right). 
abbAUX(bin(L, V, R), P, right) :-
    V > P, 
    abbAUX(L, V, left),
    abbAUX(R, V, right). 

/* bin(bin(bin(nil, 40, nil), 50, bin(nil, 55, nil)), 60, bin(nil, 100, bin(nil, 200, bin(bin(nil, 240, nil), 250, bin(nil, 255, nil))))) */

abbInsertar(X, nil, bin(nil, X, nil)).
abbInsertar(X, bin(L, N, R), bin(L, N, TRI)) :-
    X > N, 
    abbInsertar(X, R, TRI), !.
abbInsertar(X, bin(L, N, R), bin(TRI, N, R)) :-
    X < N, 
    abbInsertar(X, L, TRI), !.  % el corte es para que deje de buscar, asi te tira la unica sol que tiene


