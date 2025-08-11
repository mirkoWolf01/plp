nat(0).
nat(X) :-
    nat(Y),
    X is Y + 1.

%! cuadroSemiMagico(+N, -XS)
cuadroSemiMagico(N, XS) :-
    length(XS, N),
    allLenN(XS, N),
    nat(Suma),
    llenarCuadro(XS, Suma),
    allSum(XS, Suma).
    

llenarCuadro([], _).
llenarCuadro([X|XS], Suma) :-
    llenarFila(X, Suma),
    llenarCuadro(XS, Suma).

llenarFila([], _).
llenarFila([X|XS], Suma) :-
    between(0, Suma, X),
    llenarFila(XS, Suma).

allLenN([], _).
allLenN([X|XS], N) :-
    length(X, N),
    allLenN(XS, N).
    
allSum([], _).
allSum([X|XS], N) :-
    sum_list(X, N),
    allSum(XS, N).
    

