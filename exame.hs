qui::Int->String
qui n = show n

data Prop = Const Bool -- constante
    | Atrib Char
    | Neg Prop -- negação
    | Conj Prop Prop -- conjunção
    | Disj Prop Prop -- disjunção
    | Impl Prop Prop -- implicação
    deriving (Eq,Show)


valor::Prop->Bool
valor (Const k) = k
valor (Neg k) = not (valor k)
valor (Conj k s) 
    |valor (k) == False && valor(s) == False = False
    |otherwise = True

valor (Disj k s) 
    |valor (k) == True && valor(s) == True = True
    |otherwise = False

valor (Impl k s) 
    |valor (k) == True && valor(s) == False = False
    |otherwise = True
    

data Arv a =  No a (Arv a) (Arv a)
              | Vazia 
              deriving (Show)


listarbfs::Arv a ->[a]
listarbfs Vazia = []
listarbfs (No x esq dir) = [x] ++ listarbfs esq ++listarbfs dir


partirantes :: Eq a => a -> [a] -> [a]
partirantes _ [] = []
partirantes _ [x] = [x]
partirantes a (x:y:xs)
    | x /= a= x : partirantes a (y:xs)
    | otherwise = partirantes a []

partirdepois :: Eq a => a -> [a] -> [a]
partirdepois _ [] = []
partirdepois a (x:xs) 
    |partirantes a (x:xs) /= [] = partirdepois a (xs)
    |otherwise = [a]++xs

partirtotal:: Eq a => a -> [a] -> ([a],[a])
partirtotal a (xs) = (partirantes a xs, partirdepois a xs)

zip33::Eq a => [a]->[b]->[c]->[(a,b,c)]
zip33 [] [] [] = []
zip33 _ _ [] = []
zip33 [] _ _ = []
zip33 _ [] _ = []
zip33 (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip33 xs ys zs

diferentes::Eq a =>[a]->[a]
diferentes [] = []
diferentes [x] = []
diferentes(x:y:xs) 
    |x/=y = [x] ++ diferentes(y:xs)
    |otherwise = diferentes(y:xs)


pitagoricos::Int ->Int->Int->Bool
pitagoricos a b c 
    |a > b && a> c && (a*a == b*b+c*c) = True
    |b> a && b>c && (b*b == a*a+c*c) = True
    |c> a && c>b && (c*c == a*a+b*b) = True
    |otherwise = False
    
hipot::Float->Float->Float
hipot a b = sqrt(a*a+b*b)


sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esq dir) = x + sumArv esq + sumArv dir

inside::Eq a =>[a] -> a -> Bool
inside [] k= True
inside (x:xs) k
    |x == k = False
    |otherwise = inside xs k


nivel :: Int -> Arv a -> [a]
nivel k Vazia = []
nivel k (No x esq dir) = (nivel (k-1)  esq) ++ (nivel (k-1) dir)
nivel 0 (No x esq dir) = [x]

contapala::String->Int
contapala [] = 0
contapala [s] = 1
contapala (x:xs) 
    |x == ' ' && xs /= [] && head(xs) /= ' ' = 1 + contapala xs
    |otherwise= contapala xs

contaletra::String->Int
contaletra [] = 0
contaletra [x] = 2
contaletra (x:xs) = 1+ contaletra xs 


makearv::String->Arv Char
makearv []  = Vazia
makearv (x:xs) = No x (makearv xs) Vazia

toList :: Arv a -> [a]
toList Vazia = []
toList (No x esq dir) = toList esq ++ [x] ++ toList dir

pesq:: Eq a =>[a] -> a ->Bool
pesq [] k = False
pesq (x:xs) k 
    |x == k = True
    |otherwise = pesq xs k

pesq2::Eq a => [a]->Bool
pesq2 [] = True
pesq2 (x:xs) 
    |(pesq (xs)  x ) == False  = pesq2 xs 
    |otherwise = False

checkasc::Ord a =>[a]->Bool
checkasc [] = True
checkasc [x] = True
checkasc (x:y:xs)
    |x < y = checkasc (y:xs)
    |otherwise = False

insert1::Ord a =>Arv a ->a ->Arv a
insert1 Vazia k = (No k Vazia Vazia)
insert1 (No x dir esq) k 
    |k > x = insert1 dir k
    |k < x = insert1 esq k
    |otherwise = (No x dir esq)


