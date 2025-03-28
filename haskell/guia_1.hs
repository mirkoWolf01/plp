--------------------Ej1--------------------
{-
I.
max2 :: (Float, Float) -> Float
normaVectorial :: (Float, Float) -> Float
substract :: Float -> Float -> Float
predecesor :: Float -> Float  
evaluarEnCero :: (Float -> Float)-> Float
dosVeces :: (a -> a) -> a -> a
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipRaro :: b -> (a -> b -> c) -> a -> c

Para muchos casos en vez de poner Float, se deberia poner Num a -> a con el fin de que sea mas general
II. Las que no estan currificadas son las siguientes: max2, C

max2' :: Float -> Float -> Float
max2' x y   | x >= y    = x
            | otherwise = y

normaVectorial' :: Float -> Float -> Float
normaVectorial' = \x y -> sqrt (x^2 + y^2)
-}

--------------------Ej2--------------------
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y    = f (x, y) 

un_curry' :: (a -> b -> c) -> (a, b) -> c
un_curry' f (x,y) = f x y