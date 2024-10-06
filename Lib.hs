
separate::Integer ->String 
separate n = show n

listada:: String->[Integer]
listada n = map (read . (:"")) n

dobrosegundo::[Integer ]->[Integer  ]
dobrosegundo [] = []
dobrosegundo [n] = [n]
dobrosegundo (x:y:xs) =  x:y*2:dobrosegundo xs 

lego:: Integer ->[Integer ]
lego n = reverse (listada (separate n))

ln:: Integer ->[Integer] 
ln n = dobrosegundo(lego n)

volta :: [Integer] -> Integer
volta = read . concatMap show

final :: Integer->Integer 
final n = sum(lego(volta(ln n))) 

validar:: Integer  ->Bool 
validar n  | final n `mod` 10 == 0           = True
           | otherwise                       = False 


curta::[a]->Int 
curta [] = 0 
curta (x:xs) = 1 + curta(xs)

divprov::Int->[Int]
divprov 1 = [1] 
divprov n = [x | x<-[1..n-1], n `mod` x == 0 ]

check:: Int-> Bool
check n  |head (divprov (n) ) == 1 && length (divprov(n)) == 1  = True
         |otherwise                     = False 


addlista::[Int]->[Int]->[Int ]
addlista []  ys = ys
addlista xs []  = xs 
addlista (x:xs) (y:ys) = (x+y):addlista xs ys

dividers:: Int->[Int ]
dividers n = [x | x<- [1..n-1], n `mod` x == 0]

divtest :: Int->Bool 
divtest n = if sum(dividers n )  == 1 then True else False  

decompor::Int->[Int]
decompor 0 = []
decompor n  | n >= 200  = 200 : decompor(n-200)
            | n >= 100  = 100 : decompor(n-100)
            | n >= 50   = 50  : decompor(n-50)
            | n >= 20   = 20  : decompor(n-20)
            | n >= 10   = 10  : decompor(n-10)
            | n >= 5    = 5   : decompor(n-5)
            | otherwise =  [] 


decomporf ::  Int -> [Int] -> [Int]
decomporf 0 xs = []
decomporf n []  = []
decomporf n (x:xs) | n < 5     = []
                   | n >=x     = x:decompork (n-x) xs
                   | otherwise = decompork n xs
 
decompork:: Int ->[Int]->[Int]
decompork 0 xs = xs
decompork n []  = []
decompork n (x:xs) | n < x     = x:decompork n xs
                   | otherwise = decompork (n-x) xs

decomporTrans :: Int -> [Int] -> ([Int], [Int])
decomporTrans n (x:xs) = (decomporf n xs , decompork n xs)

soma2 ::  Int->Int->Int
soma2 a b = (a+b)

teste1 :: Int->[Int]
teste1 n =  [x | x<-[1..n], x`mod`2/=0]

fibs :: [Integer]
fibs = 0 : 1 : [a+b | (a,b)<-zip fibs (tail fibs)]



gayfinal::Int->[String]
gayfinal n = gay2(gay(n))

fibonacci1::Int->Int
fibonacci1 0 = 0
fibonacci1 1 = 1
fibonacci1 n = fibonacci1 (n-1) + fibonacci1 (n-2)

lf::Int->[Int]
lf n = [fibonacci1(x) | x<-[0..n-1]]

divprove::Int->[Int]
divprove n = [x | x<-[1..n], n`mod`x == 0 ]

primalidade:: Int->Bool
primalidade n | length(divprove (n))==2 && head(divprove (n))== 1 = True
              | otherwise = False

myand :: [Bool]->Bool
myand [] = True
myand (x:xs) | x == True = myand xs
             | otherwise = False

myor::[Bool]->Bool
myor [] = False
myor (x:xs) | x == False = myand xs
             | otherwise = True

flatten::[[a]]->[a]
flatten [] = []
flatten ([]:vs) = flatten vs
flatten ((x:xs):vs) = x:flatten (xs:vs)

makel:: a->[a]
makel n = [n] ++ makel n

makelist:: Int->a-> [a]
makelist 0 k = []
makelist n k = k : makelist (n-1) k

listnumber::[a]->Int->a
listnumber (x:xs) 1 = x
listnumber (x:xs) n = listnumber (xs) (n-1)

mudar::Int->[Int]->[Int]
mudar n [] = []
mudar n (x:xs) = n : mudar (n) (xs) 

rep::Eq a => a->[a]->[a]
rep n [] = []
rep n (x:xs) | n == x = rep n (xs)
             | n /= x = x : rep n (xs) 

qui::Int->String
qui n = show n

pic::String->[Int]
pic n = map (read . (:"")) n

carlitos::Int->[Int]
carlitos n = pic(qui n)

algarismos1:: Int->[Int]
algarismos1 0 = []
algarismos1 n = [n `mod` 10] ++ algarismos1 (n`div`10)

inverte:: [Int]->[Int]
inverte [] = []
inverte (x:xs) = inverte (xs) ++ [x] 

alga::Int->[Int]
alga n | n == 0 = [0]
       | otherwise = inverte(algarismos1(n))

bin1:: Int->[Int]
bin1 0 = []
bin1 n = [n`mod`2] ++ bin1(n`div`2)

fbits:: [Int]->Int
fbits [] = 0
fbits [1] = 1
fbits (x:xs) | x == 1 = 2^(length (xs)) + fbits (xs)
             | x == 0 = 0 +fbits (xs)
             | otherwise = 0

mal::Int->[Int]
mal n = [1..n]


crl::String->String
crl [] = []
crl (x:xs) = "-" ++ crl xs

type Assocz k v  = [(k,v)]

key::Int->(Int,String)->Bool
key n (x,s) | n == x   =  True
            | otherwise = False

printa:: (Int,String)->String
printa (n,s) = s


find1:: Int ->Assoc Int String->String 
find1 n [] = "errou miseravi"
find1 n (x:xs) | key n x == True  =  printa(x)
              | otherwise = find1 n xs 

data Prop = Const Bool        -- constantes
          | Var Char          -- variáveis
          | Neg Prop          -- negação
          | Conj Prop Prop    -- conjunção
          | Disj Prop Prop    -- disjunção
          | Equiv Prop Prop   -- equivalencia
          | Impl Prop Prop    -- implicação
            deriving (Eq,Show)

-- listas de associações
type Assoc ch v = [(ch,v)]


-- encontrar o valor associado a uma chave
-- parcial: erro se a chave não ocorre
find :: Eq ch => ch -> Assoc ch v -> v
find x assocs = head [v | (x',v)<-assocs, x==x']


-- atribuições de valores a variaveis
type Atrib = Assoc Char Bool

-- calcular o valor duma proposição
valor :: Atrib -> Prop -> Bool
valor s (Const b) = b
valor s (Var x)    = find x s
valor s (Neg p)    = not (valor s p)
valor s (Conj p q) = valor s p && valor s q
valor s (Disj p q) = valor s p || valor s q
valor s (Impl p q) = not (valor s p) || valor s q
valor s (Equiv p q) = not((valor s p || valor s q) && (not(valor s p && valor s q)))

gen :: Int -> [[Bool]]
gen 0 = [[]]
gen n = [b:bs | bs<-gen (n-1), b<-[False,True]]

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Neg p) = vars p
vars (Conj p q) = vars p ++ vars q
vars (Disj p q) = vars p ++ vars q
vars (Impl p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q


tiraconst:: String->String
tiraconst "" = ""
tiraconst (x:xs) = [x] ++ tiraconst ( [k | k <- xs , k /= x ]) 

satisfaz:: Prop->Bool
satisfaz (Const b) = b 

atribs :: Prop -> [Atrib]
atribs p = map (zip vs) (gen (length vs))
      where vs = tiraconst (vars p)

takefish::[Atrib]->Atrib
takefish (x:xs) = x 

takegen::Prop->[Atrib]->[Bool]
takegen s [] = []
takegen s (x:xs) = [valor x s ] ++ (takegen s xs) 

allprop::Prop->[Bool]
allprop s = takegen s (atribs s ) 

istaut1::[Bool]->[Bool]
istaut1 [] = [True]
istaut1 (x:xs) | x == True = istaut1 xs 
               | otherwise =  [False]

istaut2::[Bool]->Bool
istaut2 [s] = s


istaut::Prop->Bool
istaut s = istaut2(istaut1(allprop s))

satisfazivel1::[Bool]->[Bool]
satisfazivel1 [] = [False]
satisfazivel1 (x:xs) | x == False  = satisfazivel1 xs 
                     | otherwise =  [True]
satisfazivel::Prop->Bool
satisfazivel s = istaut2(satisfazivel1(allprop s))

equalitaris1::[Bool]->[Bool]->[Bool]
equalitaris1 [] [] = [True]
equalitaris1 (x:xs) (f:fs) | x == f  = equalitaris1 xs fs
                           | otherwise =  [False]

equalitaris::Prop->Prop->Bool
equalitaris s n = istaut2(equalitaris1 (allprop s) (allprop n))


removehead::[Int]->[Int]
removehead (x:xs) = xs

maximidade1::[Int]->[Int]
maximidade1 [] = []
maximidade1 [n] = [n]
maximidade1 (x:xs)      | x > head xs = maximidade1(x:(removehead(xs)))   
                        | otherwise = maximidade1 xs 

maximidade::[Int]->Int
maximidade s = head(maximidade1 s) 


data Arv a =  No a (Arv a) (Arv a)
              | Vazia 
              deriving (Show)

listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

ascendente1 ::Ord a => [a] -> Bool
ascendente1 [] = True
ascendente1 [_] = True
ascendente1 (x:y:xs) | x < y = ascendente1 (y:xs) 
                     | otherwise = False

ascendente:: Ord a => Arv a ->Bool
ascendente n = ascendente1(listar(n))

procurar :: Ord a => a -> Arv a -> Bool
procurar x Vazia = False -- não ocorre
procurar x (No y esq dir)
      | x==y = True -- encontrou
      | x<y = procurar x esq -- procura à esquerda
      | x>y = procurar x dir -- procura à direita

inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir)
        | x==y = No y esq dir -- já ocorre; não insere
        | x<y = No y (inserir x esq) dir -- insere à esquerda
        | x>y = No y esq (inserir x dir) -- insere à direita

-- Construir uma árvore equilibrada
-- pré-condição: a lista de valores deve estar
-- por ordem crescente
construir :: [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir xs') (construir xs)
     where n = length xs`div`2 -- ponto médio
           xs' = take n xs -- partir a lista
           x:xs = drop n xs

nivel1 ::  Int -> Arv a -> [a]
nivel1 s ( Vazia ) = []
nivel1 0 (No x (esq) (dir)) = [x]
nivel1 s (No x (esq) (dir)) = nivel1 (s-1) (esq) ++ nivel1 (s-1) (dir) ++ [x]

nivel2 :: Int->[a] -> [a]
nivel2 s (x:xs) =  take (2^s) (x:xs)

nivel :: Int -> Arv a -> [a]
nivel s arv = nivel2 s (nivel1 s arv)

succa:: Float->Float
succa 0 = 0
succa 1 = 2
succa n = (1+(1/n)) + (succa (n-1))

















getmax::[Int]->[Int]
getmax [x] = [x]
getmax (x:y:xs) | x > y = getmax(x:xs)
                | otherwise = getmax (y:xs)



getmaxo::[Int]->Int
getmaxo [x] = x
getmaxo (x:y:xs) | x > y = getmaxo(x:xs)
                 | otherwise = getmaxo (y:xs)

removeint::Int->[Int]->[Int]
removeint s n = [k | k<-n , k /= s ]


makeordmax:: [Int]->[Int]
makeordmax [x] = [x]
makeordmax (x:xs) = getmax(x:xs) ++ makeordmax( removeint (getmaxo(x:xs)) (x:xs))


isperfeito :: Int->Bool
isperfeito n 
    |n  == sum(divprov n) = True 
    |otherwise = False

perfeitos::Int->[Int]
perfeitos 1 = [] 
perfeitos n
    |isperfeito n == True =  [n] ++ perfeitos (n-1) 
    |otherwise = [] ++ perfeitos (n-1)



tempom :: Int -> Int -> Int -> Int -> Int
tempom h1 m1 h2 m2 = (h2 * 60 + m2) - (h1 * 60 + m1)

horas :: Int -> Int
horas k = k `div` 60

minutos :: Int -> Int
minutos k = k `mod` 60

segundos :: Int -> Int
segundos k = k * 60

dialogo :: Int -> Int -> Int -> Int -> (String, String)
dialogo h1 m1 h2 m2
  | tempom h1 m1 h2 m2 == 1 = ("Passou apenas 1 minuto!", "Queres dizer, 60 segundos?!")
  | minutos (tempom h1 m1 h2 m2) == 1 && horas (tempom h1 m1 h2 m2) /= 1 = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (horas (tempom h1 m1 h2 m2)) ++ " horas e " ++ show (minutos (tempom h1 m1 h2 m2)) ++ " minuto?!")
  | minutos (tempom h1 m1 h2 m2) == 1  &&horas (tempom h1 m1 h2 m2) == 1 = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (horas (tempom h1 m1 h2 m2)) ++ " hora e " ++ show (minutos (tempom h1 m1 h2 m2)) ++ " minuto?!")
  | minutos (tempom h1 m1 h2 m2) == 0  &&horas (tempom h1 m1 h2 m2) == 1 = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (horas (tempom h1 m1 h2 m2)) ++ " hora?!")
  | minutos (tempom h1 m1 h2 m2) == 0   = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (horas (tempom h1 m1 h2 m2)) ++ " horas?!")
  | horas (tempom h1 m1 h2 m2) == 1 = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (horas (tempom h1 m1 h2 m2)) ++ " hora e " ++ show (minutos (tempom h1 m1 h2 m2)) ++ " minutos?!")
  | tempom h1 m1 h2 m2 >= 60  = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (horas (tempom h1 m1 h2 m2)) ++ " horas e " ++ show (minutos (tempom h1 m1 h2 m2)) ++ " minutos?!")
  | otherwise = ("Passaram apenas " ++ show (tempom h1 m1 h2 m2) ++ " minutos!", "Queres dizer, " ++ show (segundos (tempom h1 m1 h2 m2)) ++ " segundos?!")


numbertolist::Integer->[Integer]
numbertolist 0 = []
numbertolist x =((x `mod` 10): numbertolist(x `div` 10))

reverter::[Integer]->[Integer]
reverter [] = []
reverter (x:xs) = reverter(xs) ++ [x]

formato2::[Integer]->[Integer]
formato2 [] = []
formato2 [x] = [x]
formato2 (x:s:xs) = [x] ++ [s*2] ++formato2(xs)

informato::Integer->[Integer]
informato x = reverter(formato2(numbertolist(x)))

simsim::[Integer]->Integer
simsim [] = 0
simsim (x:xs) 
      |x<10 =  x + simsim(xs)
      |otherwise = simsim(numbertolist(x)) +simsim(xs)
validar1::Integer -> Bool
validar1 k
      | simsim(informato(k))`mod` 10  == 0 =True
      | otherwise = False

map2 f [] = []
map2 f (x:xs) = f x : map f xs


rismos:: Int->[Int]
rismos x 
      | x < 2  = [x]
      |otherwise = [x`mod` 2] ++ rismos(x`div`2)



rismos2::Int->[Int]
rismos2 x = reverse(rismos(x))

rismosconvert::[Int]->Int
rismosconvert [] = 0
rismosconvert (x:xs) = x * 2^(length xs) + rismosconvert xs

insertinorder :: Int -> [Int] -> [Int]
insertinorder x [] = [x]
insertinorder x (y:ys)
      | x < y     = x : y : ys
      | otherwise = y : insertinorder x ys

insertthatshit:: Int->Int-> [Int]->[Int]
insertthatshit 0 k (y:ys) = [k]++(y:ys)
insertthatshit x k (y:ys) = y : insertthatshit (x-1) k ys

aoquadrado:: [Int]->[Int]
aoquadrado [] = []
aoquadrado (x:xs) = [x^2] ++ aoquadrado xs


aoquadrado1:: [Int]->[Int]
aoquadrado1 xs = [k^2 | k<-xs ]

primo :: Integer -> Bool
primo x = x>1 && not(any teste[2..lim])
      where
            teste d = x `mod` d == 0
            lim =floor (sqrt (fromIntegral x))


zipWith1 f xs ys = [f x y | (x, y) <- zip xs ys]

zipWith2 f [] (y:ys) = []
zipWith2 f (x:xs) [] = []
zipWith2 f [] [] = [] 
zipWith2 f (x:xs) (y:ys) = [f x y] ++ zipWith2 f xs ys 

plus :: [a] -> [a] -> [a]
plus [] []  = []
plus [] (y:ys) = y: plus [] ys
plus (x:xs) (y:ys) =x: plus xs (y:ys)


concat20 :: [[a]] -> [a]
concat20 [[]] = []
concat20 [x] = x
concat20 (x:xs) = x ++ concat20 xs


main :: IO ()
main = do
    txt <- getContents
    let result = format txt
    putStr result
    
    
tiraprim::String->String
tiraprim (x:xs) =  xs



paragraphs :: String -> [String]
paragraphs "" = [""]
paragraphs (x:xs)
  | x == '\n'  && head xs == '\n' = "" : paragraphs (tiraprim xs)
  | x == '\n'  && head xs /= '\n' = "" : paragraphs (xs)
  |otherwise  = (x : head ps) : tail ps
  where ps = paragraphs xs



format :: String -> String
format x = unlines (map unwords (fillWords 70 (paragraphs x)))



fillWords :: Int -> [String] -> [[String]]
fillWords _ [] = []
fillWords n (w:ws) = (formato [w] ws n)


formato :: [String] -> [String] -> Int -> [[String]]
formato line [] _ = [reverse line]
formato line (x:xs) n
      |length  (unwords (x:line))<= n =  formato (x:line) xs n
      |otherwise = reverse line : formato [x] xs n


listing::Arv a ->[a]
listing Vazia = []
listing (No x (esq) (dir)) = listing esq ++[x] ++ listing dir