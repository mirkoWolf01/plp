rama(bin(nil, N, nil), [N]).
rama(bin(L, N, _), [N|C]) :- rama(L, C).
rama(bin(_, N, R), [N|C]) :- rama(R, C).

ramaMasLarga(AB, C) :-
    rama(AB, C), 
    length(C, I),
    not(hayMasLarga(AB, I)).

hayMasLarga(AB, I) :-
    rama(AB, C),
    length(C, J),
    J > I.

ramaUnicaDeLong(A,N,C) :-
    rama(A,C),
    length(C,N), 
    not(hayOtraRama(A,N,C)).

hayOtraRama(A,N,C) :-
    rama(A,C1),
    length(C1,N), 
    C \= C1.