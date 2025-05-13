module Solucion where

main :: IO()

--Ejercicio 1
hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar letra ((u,v):xs) = letra == u || hayQueCodificar letra xs


--Ejercicio 2
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar a frase mapeo | not (hayQueCodificar a mapeo) = 0
                                           | otherwise = apariciones a frase
apariciones :: Char -> [Char] -> Int
apariciones _ [] = 0
apariciones letra (x:xs) | letra == x = 1 + apariciones letra xs
                         | otherwise = apariciones letra xs


--Ejercicio 3
laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar [x] _ = x
laQueMasHayQueCodificar (x:xs) mapeo | cuantasVecesHayQueCodificar x (x:xs) mapeo >= cuantasVecesHayQueCodificar maximoEnCola (x:xs) mapeo = x
                                     | otherwise = maximoEnCola
                                     where maximoEnCola  = laQueMasHayQueCodificar xs mapeo


--Ejercicio 4
codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (x:xs) mapeo | hayQueCodificar x mapeo = segundoComponente x mapeo : codificarFrase xs mapeo
                            | otherwise = x:codificarFrase xs mapeo

segundoComponente :: Char -> [(Char,Char)] -> Char
segundoComponente letra [(_,v)] = v
segundoComponente letra ((u,v):xs) | letra == u = v
                        | otherwise = segundoComponente letra xs

main = do
  
  print (codificarFrase "Emerson Pereira" [('e','p'), ('s','o')]) -- esperado: "Emporron Ppirira"
  
  putStrLn "Test 2:"
  print (hayQueCodificar 'a' [('e','p'), ('s','o')]) -- esperado: False
  
  putStrLn "Test 3:"
  print (cuantasVecesHayQueCodificar 'e' "Emerson Pereira" [('e','p')]) -- esperado: 3

