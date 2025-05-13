{-
Ejercicio 4. 

a4) Definir las siguientes funciones sobre listas de caracteres, interpretando una palabra como una secuencia de
caracteres sin blancos:

a) sacarBlancosRepetidos :: [Char] -> [Char], que reemplaza cada subsecuencia de blancos contiguos de la primera lista por un solo blanco en la lista resultado.
b) contarPalabras :: [Char] -> Integer, que dada una lista de caracteres devuelve la cantidad de palabras que tiene.
c) palabras :: [Char] -> [[Char]], que dada una lista arma una nueva lista con las palabras de la lista original.
d ) palabraMasLarga :: [Char] -> [Char], que dada una lista de caracteres devuelve su palabra m´as larga.
e) aplanar :: [[Char]] -> [Char], que a partir de una lista de palabras arma una lista de caracteres concaten´andolas.
f ) aplanarConBlancos :: [[Char]] -> [Char], que a partir de una lista de palabras, arma una lista de caracteres
concaten´andolas e insertando un blanco entre cada palabra.
g) aplanarConNBlancos :: [[Char]] -> Integer -> [Char], que a partir de una lista de palabras y un entero n,
arma una lista de caracteres concaten´andolas e insertando n blancos entre cada palabra (n debe ser no negativo).

b4) ¿C´omo cambian los ejercicios si agregamos el renombre de tipos: type Texto = [Char]?

-}

main :: IO()

reAux = "No se  puede hacer       nada. Pero, podemos tomar un mate"

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (l1:l2:frase)
    | (l1 == ' ') && l2 == ' ' = sacarBlancosRepetidos (l2:frase)
    | otherwise = l1 : sacarBlancosRepetidos (l2:frase)


contarPalabras :: [Char] -> Int
contarPalabras frase = contarAux( sacarBlancosRepetidos frase )

contarAux :: [Char] -> Int
contarAux [x] = 1
contarAux (x:xs)
    | x == ' ' = 1 + contarAux xs
    | otherwise = contarAux xs


palabras :: [Char] -> [[Char]] 
palabras [] = [] 
palabras (' ':xs) = palabras xs
palabras (letra:frase) = separadorAux (letra:frase) : palabras (palabrasAux (letra:frase))

separadorAux :: [Char] -> [Char]
separadorAux [] = []
separadorAux (' ':xs) = []
separadorAux (x:xs) = x : separadorAux xs

palabrasAux :: [Char] -> [Char]
palabrasAux [] =[]
palabrasAux (' ':xs) = xs
palabrasAux (x:xs) = palabrasAux xs
main = print( palabras reAux)
-- return = 10