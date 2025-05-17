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

-- IV.
sumaAlt :: (Num a) => [a] -> a 
sumaAlt = fst . foldr (\x (res, alt) -> (if alt then res - x else res + x , not alt)) (0, False) . reverse

sumaAlt' :: (Num a) => [a] -> a 
sumaAlt' = foldr (-) 0

-- V.
sumaAltRev :: (Num a) => [a] -> a 
sumaAltRev = fst . foldr (\x (res, alt) -> (if alt then res - x else res + x , not alt)) (0, False)

--------------------Ej4-------------------- Todo
-- I.
permutaciones :: [a] -> [[a]]
permutaciones = foldr f [[]] 
                where   f = \x rec -> concatMap (\xs -> map (\i -> drop i xs ++ [x] ++ take i xs) [0.. length xs]) rec

-- Uso listas por compresion. Pero se puede hacer con map.
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = partes xs ++ [x:ys | ys <- partes xs]
-- partes (x:xs) = partes xs ++ map ((:) x) (partes xs)

prefijos :: [a] -> [[a]]
prefijos = foldl f [[]]
        where f = \rec x -> rec ++ [last rec ++ [x]]

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
a :: [[Int]]
a =     [
        [1, 1, 1],
        [2, 2, 2],
        [3, 3, 3]
        ]

b :: [[Int]]
b =     [
        [1, 1, 8, 2],
        [2, 8, 2,10],
        [5, 5, 5, 7]
        ]
                
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat ls = foldr f (const []) ls 
                where f = \xs rec yss -> zipWith (+) xs (head yss) : rec (tail yss)

transponer :: [[Int]] -> [[Int]]
transponer  = foldr (zipWith (:)) (repeat [])

-- Como zipWith corta con el tamaño del primero, dada una entrada finita va a devolver una salida finita. 
-- Luego, ingresa el valor en cada una de las listas correspondientes.

--------------------Ej9--------------------
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia b = foldNat f 1
            where f = \n rec -> b * rec 

--------------------Ej10--------------------
-- Por varias razones, este ejercicio no va a ser resuelto. 

--------------------Ej11--------------------
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

poli = Suma (Prod (Prod X X) X) (Cte 4)

evaluar :: Num a => a -> Polinomio a -> a
evaluar _ (Cte m)       = m
evaluar n X             = n
evaluar n (Suma s1 s2)  = evaluar n s1 + evaluar n s2
evaluar n (Prod p1 p2)  = evaluar n p1 * evaluar n p2 

--------------------Ej12--------------------
data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

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

alturaAB :: AB a -> Integer
alturaAB  = foldAB f 0 
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

--------------------Ej13--------------------
-- I.  

vac = Nil
arbolito = Bin (Bin (Nil) 4 (Nil)) 2 (Bin (Nil) 5 (Nil))
arbol = Bin (Bin (Bin (Nil) 1 (Nil)) 2 (Nil)) 1 (Bin (Bin (Bin (Nil) 7 (Nil)) 4 (Bin (Nil) 9 (Nil))) 3 (Bin (Nil) 5 (Bin (Nil) 10 (Nil))))

ramas :: AB a -> [[a]]
ramas = foldAB f []
        where f = \reci r recd -> if null reci && null recd then [[r]] else map (r:) (reci ++ recd) 

cantHojas :: AB a -> Int 
cantHojas = foldAB f 0
        where f = \reci _ recd -> if reci == 0 && recd == 0 then 1 else reci + recd 

espejo :: AB a -> AB a 
espejo  = foldAB f Nil
        where f = \reci r recd -> Bin recd r reci 

-- II.  
mismaEstructura :: AB a -> AB b -> Bool 
mismaEstructura ab0 ab1 = foldAB f (esNil) ab0 ab1
                        where f  = \reci _ recd arbol -> case arbol of
                                Nil -> False
                                Bin izq r der -> reci izq && reci der  

--------------------Ej14--------------------
data AIH a = Hoja a | BinAIH (AIH a) (AIH a)

aih0 = Hoja 2
aih1 = BinAIH (Hoja 1) (Hoja 5)
aih2 = BinAIH (BinAIH (Hoja 7) (Hoja 3)) (Hoja 5)
aih3 = BinAIH (BinAIH (BinAIH (BinAIH (Hoja 10) (Hoja 0)) (Hoja 1)) (Hoja 3)) (BinAIH (Hoja 2) (BinAIH (Hoja 6) (Hoja 5)))

foldAIH :: (b -> b -> b) -> (a -> b) -> AIH a -> b
foldAIH f g aih = case aih of 
        Hoja a -> g a
        BinAIH i d -> f (foldAIH f g i) (foldAIH f g d)

altura :: AIH a -> Integer
altura = foldAIH f (const 1)
        where f = \reci recd -> 1 + max reci recd

tamaño :: AIH a -> Integer
tamaño = foldAIH (+) (const 1)

--------------------Ej15--------------------

data RoseTree a = Rose a [RoseTree a]   

t0 = Rose 7 []
t1 = Rose 7 [Rose 1 [Rose 4 [Rose 4 [], Rose 20 []], Rose 9 [], Rose 299 []], Rose 2 [],Rose 3 []]
t2 = Rose 7 [Rose 1 [], Rose 2 [Rose 1 [Rose 1 [Rose 1 [Rose 1 [Rose 1 []]]]], Rose 5 [], Rose 9 []], Rose 3 []]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose x xs) = f x (map (foldRose f) xs)

hojas :: RoseTree a -> [a]
hojas = foldRose f 
        where f = \r rec -> if null rec then [r] else concat rec

distanciasI :: RoseTree a -> [Int]       -- Devuelve solo Ints
distanciasI = foldRose f
            where f = \_ rec -> if null rec then [0] else map (+1) (concat rec)

distanciasT ::  RoseTree a -> [(a,Int)]  -- Devuelve tuplas que son (hoja, altura)
distanciasT rt = zip hojasList distList
                where   hojasList = hojas rt
                        distList  = foldRose (\_ rec -> if null rec then [0] else map (+1) (concat rec)) rt

alturaRT :: RoseTree a -> Int
alturaRT = foldRose f 
        where f = \_ rec -> if null rec then 1 else (+1) $ maximum rec 

--------------------Ej16--------------------
--TODO

--------------------Ej17--------------------
--[ x | x <- [1..3], y <- [x..3], (x + y) `mod' 3 == 0 ]
-- RES = [ 1, 3]

--------------------Ej18--------------------
paresDeNat::[(Int,Int)]
paresDeNat = [ (a, b) | c <- [0..], a <- [0..c], b <- [0..c], a+b == c]

--------------------Ej19--------------------
{- 
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]

No esta buena, ya que nunca sale de a, osea b y c quedan en 1 eternamente. 
-}
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..a], c <- [1..a+b], a^2 + b^2 == c^2]
