juntar([], YS, YS).
juntar([X|XS], YS, [X|Z]) :- juntar(XS, YS, Z).