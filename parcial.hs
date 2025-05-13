module SolucionT1 where

-- Ejercicio 1
mayorSumaPosicionPares :: Integer -> Integer -> Integer
mayorSumaPosicionPares n1 n2
    | sumarDigitosPares n1 1 > sumarDigitosPares n2 1 = n1
    | sumarDigitosPares n1 1 < sumarDigitosPares n2 1 = n2
    | otherwise = n1

-- funcion auxiliar
sumarDigitosPares :: Integer -> Integer -> Integer
sumarDigitosPares 0 _ = 0
sumarDigitosPares numero i
    | mod i 2 == 0 = (mod numero 10) + sumarDigitosPares (div numero 10) (i+1)
    | otherwise = sumarDigitosPares (div numero 10) (i+1)

-- Ejercicio 2
productosAReponer :: [(String, Integer, Integer)] -> Integer -> [(String,Integer)]
productosAReponer [] _ = []
productosAReponer ((p,cod,cantP):lista) cantidad
    | cantP < cantidad = (p, cod) : productosAReponer lista cantidad
    | otherwise = productosAReponer lista cantidad

-- Ejercicio 3
hayColumnaSumaCero :: [[Integer]] -> Bool
hayColumnaSumaCero [[1]] = False

hayColumnaSumaCero matriz 
    | head matriz == [] = False
    | sumarHead matriz == 0 = True 
    | otherwise = hayColumnaSumaCero (removerHead matriz)

--fucunciones auxiliares
sumarHead :: [[Integer]] -> Integer
sumarHead [] = 0
sumarHead (x:xs) = head x + sumarHead xs

removerHead :: [[Integer]] -> [[Integer]]
removerHead [] = []
removerHead (x:xs) = tail x : removerHead xs 

-- Ejercicio 4
primerosKNumerosAritmeticos :: Integer -> [Integer]
primerosKNumerosAritmeticos 1 = [1]
primerosKNumerosAritmeticos n = numeroAritimeticos n 1 0
    where numeroAritimeticos x y z
            | x == z = []
            | mod (sumarDivisores y 1) (cantidadDivisores y 1) == 0 = y :  numeroAritimeticos x (y+1) (z+1)
            | otherwise = numeroAritimeticos x (y+1) z


-- funciones Auxiliares
sumarDivisores :: Integer -> Integer -> Integer
sumarDivisores x n
    | x == n = x
    | mod x n == 0 = n + sumarDivisores x (n+1)
    | otherwise = sumarDivisores x (n+1)

cantidadDivisores :: Integer -> Integer -> Integer
cantidadDivisores 0 _= 0
cantidadDivisores x n
    | x == n = 1
    | mod x n == 0 = 1 + cantidadDivisores x (n+1)
    | otherwise = cantidadDivisores x (n+1)


main:: IO()
main = do
    print (mayorSumaPosicionPares 5 9)       -- esperado: 5
    print (mayorSumaPosicionPares 9 5)       -- esperado: 9
    print (mayorSumaPosicionPares 37 38)     -- esperado: 37
    print (mayorSumaPosicionPares 137 238)   -- esperado: 137
    print (mayorSumaPosicionPares 12345 54321) -- esperado: 12345
    print (mayorSumaPosicionPares 132132 2140) -- esperado: 132132