-- Un número natural es perfecto cuando la suma de sus dividores es igual a él. 6 es perfecto, porque la suma de 1, 2, 3 es igual a 6
-- Son números amigos cuando al sumar los divisores próprio se obtiene el otro. 220 y 284

--divisoresPropios :: Int -> [Int]

-- ordenado de menor para mayor
-- sin elementos repetidos
-- sin elementos que no se un divisor próprio de n
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use even" #-}


divisoresPropios :: Int -> [Int]

divisoresPropios n = divisoresAux n 1
                        where divisoresAux x y 
                                | y == x = []
                                | mod x y == 0 && y < x = y : divisoresAux x (y+1)
                                | otherwise = divisoresAux x (y+1)

sumarLista :: [Int] -> Int

sumarLista [] = 0 
sumarLista (x:xs) = x + sumarLista xs 

sonAmigos :: Int -> Int -> Bool
sonAmigos n m = sumarLista (divisoresPropios n) == m && sumarLista (divisoresPropios m) == n

losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = losPerfectos n 1 0
    where losPerfectos x y z 
            | z == x = []
            | numeroPerfecto y = y: losPerfectos x (y+1) (z+1)
            | otherwise = losPerfectos x (y+1) z

numeroPerfecto :: Int -> Bool
numeroPerfecto n = sumarLista (divisoresPropios n) == n

--listaDeAmigos :: [Int] -> [(Int, Int)]

main :: IO()

main = do
    print (losPrimerosNPerfectos 4) -- 6
    print (sonAmigos  220 284) -- 6, 28
    print (sonAmigos 1184 1210) 