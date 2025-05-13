{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

main :: IO()
reEquipos = [("Tigres do Norte", "Andre Souza"), 
             ("Furia do Sul", "Emerson Silva"), 
             ("Relampago Dourado", "Pedro Albuquerque"), 
             ("Aguias da Serra", "Lucas Martins"), 
             ("Tempestade Azul", "Joao Ferreira"), 
             ("Leoes do Vale", "Carlos Eduardo"), 
             ("Guardioes da Areia", "Bruno Henrique"), 
             ("Dragoes Urbanos", "Felipe Lima")]

reGoles = [3, 1, 2, 5, 4, 6, 1, 7]

reTotalGolesTorneo = 40

{-

CONSIGNA

¡Vamos Campeones!
En exactas se está jugando un torneo de fútbol y la facultad le pidió a los alumnos de IP programar algunas
funcionalidades en Haskell, Los datos con los que contamos para esto son los nombres de los equipos que participan
del torneo, los nombres de los arqueros titulares de cada uno de dichos equipos, y la cantidad de goles recibidos
por esos arqueros. Los nombres de los equipos y sus respectivos arqueros serán modelados mediante tuplas de tipo
(String, String), donde la primera componente representa el nombre del equipo, y la segunda representa el nombre
del arquero titular de dicho equipo.
En los problemas en los cuales se reciben como parámetros secuencias arquerosPorEquipo y goles, cada posición de
la lista goles representará la cantidad de goles recibidos por el arquero del equipo que se encuentra en esa misma
posicion de arquerosPorEquipo. Por ejemplo, si la lista arquerosPorEquipo es [("Sacachispas", "Neyder Aragon"),
("Fenix", "Nahuel Galardi")] y la lista de goleses [3, 5], eso indicaría que Neyder Aragon recibió 3 goles, y
Nahuel Galardi 5.

Se pueden usar las siguientes funciones del preludio:
	- Listas: head, tail, last, init, length, elem, ++
	- Tuplas: fst, snd
	- Operaciones Lógicas: &&, ||, not
	- Constructores de listas: (x:xs), []
	- Constructores de tuplas: (x, y)


1) Atajaron Suplentes

problema atajaronSuplentes (arquerosPorEquipo: seq<String X String>, goles: seq<Z>, totalGolesTorneo: Z): Z {
	requiere: {equiposValidos(arquerosPorEquipo)
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
	asegura: {
	res es la cantidad de goles recibidos en el torneo por arqueros que no son titulares en sus equipos.
	}
}


2) Equipos Válidos

problema equiposValidos (arquerosPorEquipo: seq<String X String>): Bool {
	requiere: {True}
	asegura: {
	(res = True) <=> arquerosPorEquipo no contiene nombres de clubes repetidos, ni arqueros repetidos, ni jugadores con nombre del club
	}
}


3) Porcentaje de goles

problema porcentajeDeGoles (arquero: String, arquerosPorEquipo: seq<String X String>, goles: seq<Z>): R {
	requiere: {La segunda componente de algún elemento de arquerosPorEquipo es arquero}
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {Hay al menos un elemento de goles mayores estricto a 0}
	asegura: {
	res es el porcentaje de goles que recibió arquero sobre el total de goles recibidos por arqueros titulares
	}
}

Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como float la división entre dos
numeros de tipo Int.

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b


4) Valla Menos Vencida

problema vallaMenosVencida (arquerosPorEquipo: seq<String X String>, goles: seq<Z>): String {
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {|goles| > 0}
	asegura: {
	res es alguno de los arqueros de arquerosPorEquipo que menor goles recibió de acuerdo a goles
	}
}

-}

type NombreDelEquipo = String
type Arquero = String
type TotalGolesTorneo = Int
type Goles = [TotalGolesTorneo]
type Equipo = (NombreDelEquipo, Arquero)
type ArquerosPorEquipo = [Equipo]



atajaranSuplentes :: ArquerosPorEquipo -> Goles -> TotalGolesTorneo -> Int
atajaranSuplentes arquerosPorEquipo goles totalGolesTorneo = totalGolesTorneo - sumaDeGoles goles

-- funciones Auxiliares
sumaDeGoles :: Goles -> Int
sumaDeGoles [] = 0
sumaDeGoles (g:goles) = g + sumaDeGoles goles

-- Ejercicio 2

equiposValidos :: ArquerosPorEquipo -> Bool
equiposValidos arquerosPorEquipo = not (hayRepetidos (listaAux arquerosPorEquipo))

-- Funciones Auxiliares

listaAux :: ArquerosPorEquipo -> [String]
listaAux [] = []
listaAux (x:xs) = fst x : snd x : listaAux xs

hayRepetidos :: [String] -> Bool

hayRepetidos [] = False
hayRepetidos (x:xs)
    | repetidosAux xs x = True
    | otherwise = hayRepetidos xs

repetidosAux :: [String] -> String -> Bool
repetidosAux [] _ = False
repetidosAux (x:xs) y
    | y == x = True
    | otherwise = repetidosAux xs y

-- Ejericio 3 
porcentajeDeGoles :: Arquero -> ArquerosPorEquipo -> Goles -> Float

porcentajeDeGoles arquero arquerosPorEquipo goles = division (pos goles (longintudHasta arquerosPorEquipo arquero)) (sumaDeGoles goles)  *100

-- funciones Auxliares

pos :: [a] -> Int -> a
pos (x:_) 1 = x
pos (_:xs) n = pos xs (n-1)
pos lista n =  pos (tail lista) n

longintudHasta :: ArquerosPorEquipo -> Arquero -> Int
longintudHasta [] _ = 0
longintudHasta (equipo:arquerosPorEquipo) arquero
    | arquero == snd equipo = 1 + longintudHasta [] arquero
    | snd equipo /= arquero = 1 + longintudHasta arquerosPorEquipo arquero

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b



vallaMenosVencida :: ArquerosPorEquipo -> Goles -> Arquero
vallaMenosVencida arquerosPorEquipo goles = snd ( pos arquerosPorEquipo (indeceMenosVencido goles (menosVencido goles)))

-- funciones auxiliares
menosVencido :: [Int] -> Int
menosVencido [a] = a 
menosVencido (x:y:xs)
    | x < y = menosVencido (x:xs)
    | otherwise = menosVencido (y:xs)

indeceMenosVencido :: Goles -> Int -> Int
indeceMenosVencido [] _ = 0
indeceMenosVencido arquerosPorEquipo sofrido = buscarIndice arquerosPorEquipo sofrido 1 
    where
    buscarIndice (x:xs) s i
      | x == s = i
      | otherwise = buscarIndice xs s (i + 1)

main = print( porcentajeDeGoles "Emerson Silva" reEquipos reGoles)