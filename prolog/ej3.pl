natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, suc(Y)) :- natural(Y), menorOIgual(X, Y).
menorOIgual(X,X) :- natural(X).

/* 
II. Cuando se consulta sobre los valores (X,Y), con X definido e Y no. En esos casos se traba.
Se soluciona verificando antes que Y sea un natural, ya que sino tiene infinitos valores posibles.
 */