--------------------Ej1--------------------
{-
null :: [t] -> Bool -- Comprueba si una lista esta vacia, en caso de estarlo devuelve True
head :: [t] -> t    -- Devuelve el primer elemento de una lista
tail :: [t] -> [t]  -- Devuelve la lista sin el primero
init :: [t] -> [t]  -- Devuelve la lista sin el ultimo
last :: [t] -> t    -- Devuelve el ultimo elemento de una lista
take :: Int -> [t] -> [t]   -- Devuelve todos los elementos de la secuencia hasta el valor indicado en forma de lista
drop :: Int -> [t] -> [t]   -- Devuelve todos los elementos de la secuencia sin los primeros n elementos
(++) :: [t] -> [t] -> [t]   -- Concatena listas
concat  :: [[t]] -> [t]     -- Concatena todas las sub_listas dentro de una lista
reverse :: [t] -> [t]       -- Da vuelta la lista, el ultimo ahora es el primero y asi
elem    :: t -> [t] -> Bool -- Comprueba si el elemento esta dentro de la lista 
-}

--------------------Ej2--------------------
valorAbsoluto :: Float -> Float
valorAbsoluto a | a < 0     = -a
                | otherwise = a

bisiesto :: Int -> Bool
bisiesto a = (mod a 4 == 0) || (mod a 100 == 0 && mod a 400 == 0)

factorial :: Integer -> Integer
factorial a | a == 0    = 1
            | otherwise = a * factorial (a-1)

cantDivisoresPrimos :: Int -> Int 
cantDivisoresPrimos a   = cantDivisoresPrimosAux a 2

cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux n d  | d > n                     = 0
                            | mod n d == 0 && esPrimo d = 1 + (cantDivisoresPrimosAux redux sigP)
                            | otherwise                 = (cantDivisoresPrimosAux n sigP)
                                where redux = reducirDivisoresPrimos n d
                                      sigP  = (siguientePrimo d)

reducirDivisoresPrimos :: Int -> Int -> Int
reducirDivisoresPrimos n d  | n `mod` d == 0    = reducirDivisoresPrimos (div n d) d
                            | otherwise         = n

siguientePrimo :: Int -> Int
siguientePrimo a    | esPrimo (n)   = n
                    | otherwise     = siguientePrimo n
                        where n = a+1

esPrimo :: Int -> Bool
esPrimo a = esPrimoAux a 2

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n d  | d >= (div n 2 +1) = True
                | mod n d == 0      = False
                | otherwise         = esPrimoAux n (d+1)

--------------------Ej3--------------------

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso a = Just (1/a)

aEntero :: Either Int Bool -> Int  -- Toma de entrada (Left ...) o (Right ...)
aEntero (Right False)   = 0
aEntero (Right True)    = 1
aEntero (Left a)        = a

--------------------Ej4--------------------
limpiar :: String -> String -> String
limpiar p1 (c:[])   | not(c `elem` p1)  = [c]
                    | otherwise         = []
limpiar p1 (c:p2)   | not(c `elem` p1)  = c:(limpiar p1 p2)
                    | otherwise         = (limpiar p1 p2)

difPromedio :: [Float] -> [Float]
difPromedio x = sumarATodo (-promedio x) x

promedio :: [Float] -> Float
promedio [] = 0
promedio x  = (promedioAux x /(fromIntegral (length x)))

promedioAux :: [Float] -> Float
promedioAux []      = 0
promedioAux (x:xs)  = x + promedioAux (xs)

sumarATodo :: Float -> [Float] -> [Float]
sumarATodo 0 x           = x
sumarATodo num []        = []
sumarATodo num (x:xs)    = (num + x):(sumarATodo num xs)

todosIguales :: [Int] -> Bool
todosIguales []         = True
todosIguales (n:[])     = True
todosIguales (n:ls)   = (n == (ls !! 0))  && todosIguales ls

--------------------Ej5--------------------
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB ab  = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil              = Nil
negacionAB (Bin ab1 a ab2)  = (Bin (negacionAB ab1) (not a) (negacionAB ab2))

productoAB :: AB Int -> Int
productoAB  Nil             = 1
productoAB (Bin ab1 a ab2)  = a * (productoAB ab1) * (productoAB ab2)
