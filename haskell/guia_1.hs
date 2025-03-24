--------------------Ej1--------------------
{-
I.
max2 :: (Float, Float) -> Float
normaVectorial :: (Float, Float) -> Float
substract :: Float -> Float -> Float
predecesor :: Float -> Float  
evaluarEnCero :: Num a => (a -> b) -> b
dosVeces :: (a -> a) -> a -> a
flipAll :: 
flipRaro :: 

Para muchos casos en vez de poner Float, se deberia poner Num a -> a con el fin de que sea mas general
II.
-}

substract :: Num a => a -> a -> a
substract = flip (-) 

predecesor :: Float -> Float
predecesor = substract 1

evaluarEnCero :: Num a => (a -> b) -> b
evaluarEnCero = \f -> f 0

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip 

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f