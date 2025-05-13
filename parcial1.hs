-- parciales Haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

main :: IO()

reAux = 
  [ ("Emerson Pereira", [8, 8, 10, 8, 9])
  , ("Pablo André", [4, 8, 5, 1, 8])
  , ("Jose Martin", [4, 8, 6, 1, 2])
  , ("Larissa Gomes", [9, 7, 8, 10, 9])
  , ("Rafael Lima", [6, 5, 7, 8, 6])
  , ("Amanda Rocha", [10, 9, 10, 9, 10])
  , ("Tiago Santos", [3, 4, 6, 5, 4])
  , ("Fernanda Alves", [7, 8, 8, 7, 9])
  , ("Carlos Eduardo", [2, 3, 5, 4, 3])
  , ("Beatriz Souza", [8, 10, 9, 10, 9])
  ]

type Alumno = [Char]
type Nota = Int
type Notas = [Nota]
type Registro = [(Alumno, Notas)]

--Ejercicio 1
aprobadoMasDeNMateria :: Registro -> Alumno -> Int -> Bool

aprobadoMasDeNMateria [] _ _ = False
aprobadoMasDeNMateria (r:registro) alumno n
    | fst r == alumno = contarMayoresACuatro (snd r) >= n
    | otherwise = aprobadoMasDeNMateria registro alumno n

-- Funciones Auxiliares
contarMayoresACuatro :: Notas -> Int
contarMayoresACuatro [] = 0
contarMayoresACuatro (n:notas)
    | n < 4 = contarMayoresACuatro notas
    | otherwise = 1 + contarMayoresACuatro notas



-- Ejercicio 2
buenosAlumnos :: Registro -> [Alumno]
buenosAlumnos [] = []
buenosAlumnos (r:registro)
    | sinAplazos (snd r) && promedio (snd r) >= 8 = fst r : buenosAlumnos registro
    | otherwise = buenosAlumnos registro

-- Funciones Auxiliares
sumar :: [Int] -> Int
sumar [] = 0
sumar (x:xs) = x + sumar xs

longintud :: [a] -> Int
longintud [] = 0
longintud (x:xs) =  1 + longintud xs

promedio :: Notas -> Float
promedio notas = fromIntegral (sumar notas) / fromIntegral  (longintud notas) 

sinAplazos :: Notas -> Bool
sinAplazos [] = True
sinAplazos (n:ns) = n >= 4 && sinAplazos ns

-- Ejercicio 3
mejorPromedio :: Registro -> Alumno
mejorPromedio [(alumno, nota)] = alumno
mejorPromedio (alumno1:alumno2:registro)
    | promedio (snd alumno1) >= promedio (snd alumno2) = mejorPromedio (alumno1:registro)
    | otherwise = mejorPromedio (alumno2:registro)


-- Ejericio 4
seGraduoConHonores :: Registro -> Int -> Alumno -> Bool

seGraduoConHonores registro n alumno = aprobadoMasDeNMateria registro alumno (n-1)  &&  perteneceAlMejores alumno (buenosAlumnos registro ) && ( notaMejorPromedio registro (mejorPromedio registro) - promedioDeAlumno registro alumno < 1 )

-- funciones auxialiares

promedioDeAlumno :: Registro -> Alumno -> Float
promedioDeAlumno [] _ = 0
promedioDeAlumno (r:registro) alumno 
    | alumno == fst r = promedio (snd r)
    | otherwise = promedioDeAlumno registro alumno

notaMejorPromedio :: Registro -> Alumno -> Float
notaMejorPromedio [(x, y)] _ = promedio y
notaMejorPromedio (r:registro) alumno
   | fst r == alumno = promedio (snd r)
   | otherwise = notaMejorPromedio registro alumno


perteneceAlMejores :: Alumno -> [Alumno] -> Bool
perteneceAlMejores alumno [] = False
perteneceAlMejores alumno (m:mejores)
    | alumno == m = True
    | otherwise = perteneceAlMejores alumno mejores

main = print (seGraduoConHonores reAux 5 "Beatriz Souza")

--main = print ( notaMejorPromedio reAux (mejorPromedio reAux) - promedioDeAlumno reAux "Beatriz Souza" )