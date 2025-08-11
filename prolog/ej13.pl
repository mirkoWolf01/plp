nat(1).
nat(X) :-
    nat(Y),
    X is Y + 1.

coprimos(X, Y) :-
    nat(X),
    nat(Y),
    gcd(X, Y) =:= 1.

