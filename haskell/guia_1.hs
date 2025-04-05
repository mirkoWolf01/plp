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
pertenece n = foldr (\x res -> (x==n) || res) False 

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

-- IV. !!! Tambien se puede hacer con foldr (-) 0
sumaAlt :: (Num a) => [a] -> a 
sumaAlt = fst . foldr (\x (res, alt) -> (if alt then res - x else res + x , not alt)) (0, False) . reverse

-- V.
sumaAlt' :: (Num a) => [a] -> a 
sumaAlt' = fst . foldr (\x (res, alt) -> (if alt then res - x else res + x , not alt)) (0, False)

--------------------Ej4--------------------
-- I.
{- permutaciones :: [a] -> [[a]]
permutaciones [x]   = [[x]]
permutaciones ls    = concatMap (\x -> [[x]]) ls  -}

--------------------Ej5--------------------
elementosEnPosicionesPares :: [a] -> [a]    -- No esta hecho con recursion estructural, debido a que se modifica xs al hacer la recursion
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]             -- Esta hecho con recursion estructural, debido a que no se modifica la lista a la que se le hace recursion
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr f (id) 
            where f = \x rec ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)

--------------------Ej6--------------------
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr f []
            where f = (\x xs rec -> if x == e then xs else x:rec)

-- No funciona un esquema de recursion estructural para sacarUno porque es necesario que corte sin modificar los proximos elementos que coincidan. 

insertarOrdenado :: Ord a => a -> [a] -> [a] -- Tiene un problema con un elemento, que lo pone atras. Sino funca bien.
insertarOrdenado e = recr f [e]
                    where f = (\x xs rec -> if x >= e then e:x:xs else x:rec)

--------------------Ej7--------------------
-- I.  
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares  f = foldr g []
            where g = \pair rec -> uncurry f pair : rec

-- II.
armarPares :: [a] -> [b] -> [(a,b)]
armarPares  = foldr f (const [])
            where f = \x rec ys -> if null ys then [] else (x, head ys): rec (tail ys) 

-- III.
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f  = foldr g (const [])
            where g = \x rec ys -> if null ys then [] else f x (head ys) : rec (tail ys)

--------------------Ej8--------------------


--------------------Ej9--------------------
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia b = foldNat f 1
            where f = \n rec -> b * rec 

--------------------Ej12--------------------
data AB a = Nil | Bin (AB a) a (AB a) deriving Show

-- I.  
foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB _ z Nil          =  z
foldAB f z (Bin i r d)  = f (foldAB f z i) r (foldAB f z d)

recAB :: (b -> a -> b -> AB a -> AB a -> b) -> b -> AB a -> b
recAB _ z Nil           =  z
recAB f z (Bin i r d)   = f (recAB f z i) r (recAB f z d) i d

-- II.  
esNil :: AB a -> Bool
esNil Nil   = True
esNil ab    = False

altura :: AB a -> Integer
altura  = foldAB f 0 
        where f = \reci _ recd ->  max reci recd + 1

cantNodos :: AB a -> Integer
cantNodos  = foldAB f 0 
            where f = \reci _ recd -> reci + recd + 1

-- III.  
mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin i r d)= foldAB g r (Bin i r d)
            where g = \reci r recd ->   if f r reci && f r recd then r else if (f reci r) && (f reci recd) then reci else recd

-- IV.  
esABB :: Ord a => AB a -> Bool
esABB = recAB (\reci r recd i d -> lABB r i && rABB r d && recd && reci ) True 

rABB :: Ord a => a -> AB a -> Bool
rABB _ Nil = True
rABB x (Bin _ y _) = x <= y

lABB :: Ord a => a -> AB a -> Bool
lABB  _ Nil = True
lABB  x (Bin _ y _) = y <= x


--------------------Ej15--------------------

data RoseTree a = Rose a [RoseTree a]   