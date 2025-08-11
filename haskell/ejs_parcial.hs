import Data.List

data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a) a (ABNV a)

-- Ejercicios de ABNV
abnv = Bi (Uni 2 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))

foldABNV :: (a -> b) -> (a-> b -> b) -> (b-> a ->b -> b) -> ABNV a -> b
foldABNV fH fUni fBi ab = case ab of 
    Hoja x          -> fH x 
    Uni  x abu      -> fUni x (r abu)
    Bi abl x abr    -> fBi (r abl) x (r abr)
    where r = foldABNV fH fUni fBi

recABNV :: (a -> b) -> (a-> b -> ABNV a -> b) -> (b-> a ->b -> ABNV a -> ABNV a-> b) -> ABNV a -> b
recABNV fH fUni fBi ab = case ab of 
    Hoja x          -> fH x 
    Uni  x ab       -> fUni x (r ab) ab
    Bi abl x abr    -> fBi (r abl) x (r abr) abl abr
    where r = recABNV fH fUni fBi

nivel :: ABNV a -> Int -> [a]
nivel = foldABNV fH fUni fBi
        where   
            fH      = (\e i  -> if i == 0 then [e] else [])
            fUni    = (\e ru i -> if i == 0 then [e] else ru (i-1))
            fBi     = (\ri e rd i -> if i == 0 then [e] else (ri (i-1)) ++ (rd (i-1)))



-- Ejercicios de Operador
data Operador = Sumar Int | DividirPor Int | Secuencia [Operador] deriving Show

foldOperador :: (Int -> a) -> (Int -> a) -> ([a] -> a) -> Operador -> a
foldOperador fSumar fDividir fSecuencia op =
  case op of
    Sumar n         -> fSumar n
    DividirPor n    -> fDividir n
    Secuencia ops   -> fSecuencia (map (foldOperador fSumar fDividir fSecuencia) ops)

aplanar :: Operador -> Operador
aplanar (Secuencia xs) = Secuencia (concatMap f xs)
    where f = (foldOperador (\x -> [Sumar x]) (\x -> [DividirPor x]) (\xs -> concat xs))
aplanar x =  x

falla :: Operador -> Bool
falla = foldOperador fS fD fSec    
    where 
        fS      = (\_ -> False)
        fD      = (== 0)
        fSec    = (\r -> elem True r)

componerTodas :: [a -> a] -> (a -> a) 
componerTodas = foldl (.) (id)

aplicar :: Operador -> Int -> Maybe Int
aplicar op i = if (falla op) then Nothing else Just (aplicarAux (aplanar op) i)

aplicarAux :: Operador -> Int -> Int 
aplicarAux op i = componerTodas (reverse (foldOperador fS fD fSec op)) i
    where 
        fS      = \x -> [(+x)]
        fD      = \x -> [(\n -> div n x)]
        fSec    = concat 


-- Ejercicios de AT

data AT a = NilT | Tri a (AT a) (AT a) (AT a) deriving Show

at1 = Tri 2 (NilT) (NilT) (NilT)
at2 = Tri 7 (Tri 12 (Tri 9 (NilT) (NilT) (NilT)) (Tri 21 (NilT) (NilT) (NilT)) (NilT)) (NilT) (Tri 10 (NilT) (NilT) (NilT))

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT z _ NilT = z
foldAT z f (Tri x lt mt rt) = f x (rec lt) (rec mt) (rec rt)
    where rec = foldAT z f 

preorder :: AT a -> [a]
preorder = foldAT [] (\x lls mls rls -> x:(lls ++ mls ++rls))

mapAT :: (a -> b) -> AT a -> AT b 
mapAT f = foldAT NilT (\x lres mres rres -> Tri (f x) lres mres rres)

nivelAT :: (AT a) -> Int -> [a]
nivelAT at n = foldAT (const []) (\x lres mres rres i -> if n == i then [x] else lres (i+1) ++ mres (i+1) ++ rres (i+1)) at 0


-- Ejercicios de Prop
data Prop  = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop
type Valuacion = String -> Bool 

prop0 = No (Var "P")
prop1 = Imp (No (Var "P")) (No (Var "Q"))
prop2 = O (Imp (No (Var "P")) (No (Var "Q"))) (Imp (Var "P")(Var "Q"))
prop3 = Y (Y (Var "P") (Y (Var "Q") (Y (Var "R") ((Var "S"))))) (Var "T") 

foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop -> b
foldProp fVar fNot fAnd fOr fImp pr = case pr of 
    Var str         -> fVar str
    No prop         -> fNot (rec prop)
    Y iprop rprop   -> fAnd (rec iprop) (rec rprop)
    O iprop rprop   -> fOr  (rec iprop) (rec rprop)
    Imp iprop rprop -> fImp (rec iprop) (rec rprop)
    where rec = foldProp  fVar fNot fAnd fOr fImp

recProp :: (String -> b) -> (b -> Prop -> b) -> (b -> b -> Prop -> Prop -> b) -> (b -> b -> Prop -> Prop -> b) -> (b -> b -> Prop -> Prop -> b) -> Prop -> b
recProp fVar fNot fAnd fOr fImp pr = case pr of 
    Var str         -> fVar str
    No prop         -> fNot (rec prop) prop
    Y iprop rprop   -> fAnd (rec iprop) (rec rprop) iprop rprop
    O iprop rprop   -> fOr  (rec iprop) (rec rprop) iprop rprop
    Imp iprop rprop -> fImp (rec iprop) (rec rprop) iprop rprop
    where rec = recProp fVar fNot fAnd fOr fImp

variables :: Prop -> [String]
variables = foldProp (flip (:) []) (id) (union) (union) (union)

valu "N" = False
valu _   = True

evaluar :: Valuacion -> Prop -> Bool
evaluar val = foldProp (val) (not) ((&&)) ((||)) ((\p1 p2 -> (not p1) && p2))

estaEnFNN ::Prop -> Bool 
estaEnFNN = recProp (const True) fNot g  g (\_ _ _ _ -> False)
    where 
        fNot = \recp prop -> case prop of
            Var _ -> True
            otherwise -> False
        g = (\reci recd _ _ -> reci && recd)


-- Ejercicio Final 1.
foldu ::  b -> (c -> b -> b) -> [b -> c] -> b
foldu z f [] = z
foldu z f (x:xs) = f (x (foldu z f xs)) (foldu z f xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z xs = foldu z (\x rec -> f x rec) (map const xs)

foldu' :: b -> (c -> b -> b) -> [b -> c] -> b
foldu' z f xs = foldr (\x rec -> f (x rec) rec) z xs

