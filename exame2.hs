import Data.Char

readnumber::String->Int
readnumber k = read k

-- completar esta definição
(+++) :: [a] -> [a] -> [a]
xs +++ ys = foldr f z lista
    where f     = ( :)
          z     = xs
          lista = ys