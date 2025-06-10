aplanar([], []).
aplanar([X|XS], L) :- 
    aplanar(X, XP), 
    aplanar(XS, XSP),
    append(XP, XSP, L).
aplanar(X, [X]) :- not(is_list(X)).
