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

--------------------Ej3--------------------
-- I.
suma :: (Num a) => [a] -> a 
suma    = foldr (+) 0 

pertenece :: (Eq a) => a -> [a] -> Bool
pertenece n = foldr (\x res -> (x==n) || res) False -- f x1 (f x2 (f x3  False))

concatenar :: [a] -> [a] -> [a]
concatenar s1 s2    = foldr (:) s2 s1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p   = foldr (\x res -> if p x then x:res else res) []

map' :: (a -> b) -> [a] -> [b]
map' f  = foldr (\x res -> f x :res ) []

-- II.
mejorSegun :: (a -> a -> Bool) -> [a] -> a 
mejorSegun f    = foldr1 (\x res -> if f x res then x else res)

-- III.
sumasParciales :: Num a => [a] -> [a]   -- Esta hecho con foldr
sumasParciales ls   = fst $ foldr (\x (res, lft) ->( x + sum lft : res, init lft)) ([], init ls) ls

sumasParciales' :: Num a => [a] -> [a]  -- Esta hecho con foldl
sumasParciales'     = reverse . fst . foldl (\(res, acc) x ->( x + acc : res, x + acc)) ([], 0) 

-- IV.
sumaAlt :: (Num a) => [a] -> a 
sumaAlt = fst . foldr (\x (res, alt) -> (if alt then res - x else res + x , not alt)) (0, False) . reverse

-- V.
sumaAlt' :: (Num a) => [a] -> a 
sumaAlt' = fst . foldr (\x (res, alt) -> (if alt then res - x else res + x , not alt)) (0, False)

--------------------Ej4--------------------

--------------------Ej5--------------------