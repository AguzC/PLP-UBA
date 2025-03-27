--3.
--I. Redefinir usando foldr las funciones sum, elem, (++), filter y map.

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

Si xs = [x1, x2, x3] entonces:
foldr f z xs = f x1 (f x2 (f x3 z))

Equivalentemente con notaci´on infija:
foldr ⋆ z xs = x1 ⋆ (x2 ⋆ (x3 ⋆ z))
-}

sumFoldr :: (Num a) => [a] -> a
sumFoldr = foldr (+) 0
--sumFoldr = foldr (\x rec -> x + rec) 0, son lo mismo

elemFoldr :: (Eq a) => a -> [a] -> Bool
--elemFoldr y = foldr (\x rec -> x == y || rec) False
elemFoldr y = foldr ((||) . (==y)) False --Cualquiera de las 2 funca,(||) . (==y) es = a 
-- hacer || (x==y) kinda.

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x rec -> if p x then x : rec else rec) []

mapFoldr :: (a -> b) -> [a] -> [b]
--mapFoldr f = foldr (\x rec -> f x : rec) []
mapFoldr f = foldr ((:).f) []

--II.
--Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
--de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>).

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec) 

--foldr1, variante de foldr que no tiene caso base, entonces solo se puede aplicar a estructuras 
--no vacias, es una funcion parcial debido a eso.

--III. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
--otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
--desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] -> [1,5,4,4,9].

-- 1 = 1, 1 + 4 = 5, 1 + 4 + (-1) = 4, 1 + 4 + (-1) + 0 = 4, 1...+ 5 = 9
--En cada caso tengo que devolver la suma de los lugares anteriores.
--El primer lugar tiene la suma de los elementos de [0] a [0],
-- Digo a = [1,4,-1,0,5]
-- sumasParciales a = [sum (take 1 a) , sum (take 2 a) , sum (take 3 a) , sum (take 4 a) , sum (take 5 a)]

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = reverse (foldr (\x rec -> sum (take (length rec + 1) xs)  :rec ) [] xs)

--Problema: Necesito que el primer argumento del take vaya aumentando, puedo usar la longitud de xs que 
-- va variando(?,0..............

sumasParciales1 :: (Num a) => [a] -> [a]
sumasParciales1 = foldr (\x rec -> x : sumarX x rec) []

sumarX :: (Num a) => a -> [a] -> [a]
sumarX x = map (+ x)


--IV. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
--resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
--Poco vale este porq me lo spoilee

sumaAlt:: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

--V.Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
--  etc.). Pensar qué esquema de recursión conviene usar en este caso.

sumaAlt2:: (Num a) => [a] -> a
sumaAlt2 xs = foldr (-) 0 (reverse xs)
--Quizas el reverse aca es trampa, god knows.


--4.
--I. Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. 
--Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.
-- ej: [1,2,3] , f [1,2,3] = [[1,2,3][1,3,2][2,1,3][2,3,1][3,1,2][3,2,1]]

--permutaciones:: (Eq a) => [a] -> [[a]]
--permutaciones xs = foldr (\x rec -> combiUnicoElem x (length xs) (filter (/=x) xs) ++ rec) [] xs
--permutaciones xs = foldr (\x -> concatMap id (combiUnicoElem x (length xs) (filter (/=x) xs))) [] xs
--permutaciones xs = foldr (concatMap . combiUnicoElem length xs) [] 
--permutaciones xs = concatMap (foldr (\x rec -> combiUnicoElem x (length xs) (filter (/=x) xs)) ++ rec) [] xs)
--permutaciones xs = concatMap (foldr foo [] xs)
--foo = (\x rec -> combiUnicoElem x (length xs) (filter (/=x) xs)) ++ rec)

--concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
--combiUnicoElem :: a -> [a] -> [[a]]

permutaciones:: (Eq a) => [a] -> [[a]]
permutaciones xs = foldr (concatMap . combiUnicoElem (length xs) ) [[]] xs

permutaciones3:: (Eq a) => [a] -> [[a]]
permutaciones3 xs = foldr (concatMap . intercalarElem) [[]] xs

combiUnicoElem:: Int -> a -> [a] -> [[a]] --Pre: a no pertenece a la lista
combiUnicoElem 0 x xs = [x : xs]
combiUnicoElem long x xs = (take long xs ++ [x] ++ drop long xs) : combiUnicoElem (long - 1) x xs  


permutaciones2:: [a] -> [[a]]
--permutaciones2 = foldr (concatMap.intercalarElem) [[]]
permutaciones2 = foldr (\x -> concatMap (intercalarElem x)) [[]]

intercalarElem :: a -> [a] -> [[a]]
intercalarElem e xs = [take i xs ++ [e] ++ drop i xs | i <- [0 .. length xs]]


{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

Si xs = [x1, x2, x3] entonces:
foldr f z xs = f x1 (f x2 (f x3 z))

Equivalentemente con notaci´on infija:
foldr ⋆ z xs = x1 ⋆ (x2 ⋆ (x3 ⋆ z))
-}
