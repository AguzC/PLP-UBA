{-# OPTIONS_GHC -Wno-x-partial #-}
import Data.List (nub)


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
--mapFoldr f xs = foldr (\x rec -> f x : rec) [] xs
mapFoldr f = foldr ((:).f) []

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

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

{- permutaciones:: (Eq a) => [a] -> [[a]]

permutaciones xs = foldr (\x -> concatMap (combiUnicoElem (length xs)) xs) [[]] xs



combiUnicoElem:: Int -> a -> [a] -> [[a]] --Pre: a no pertenece a la lista
combiUnicoElem 0 x xs = [x : xs]
combiUnicoElem long x xs = (take long xs ++ [x] ++ drop long xs) : combiUnicoElem (long - 1) x xs  

combiUnicoElem':: a -> [a] -> [[a]]
combiUnicoElem' x xs = foldr (\x xs -> take (length xs) xs ++ [x] ++ drop (length xs) xs) [] xs
 -}
permutaciones2:: [a] -> [[a]]
permutaciones2 = foldr (\x -> concatMap (intercalarElem x)) [[]]

intercalarElem :: a -> [a] -> [[a]]
intercalarElem e xs = [take i xs ++ [e] ++ drop i xs | i <- [0 .. length xs]]



--5. 


--Considerar las siguientes funciones y indicar si la recursión utilizada en cada una de ellas es o no estructural. 
--Si lo es, reescribirla utilizando foldr, En caso contrario, explicar el motivo.

--a
{- elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                       then [x]
                                       else x : elementosEnPosicionesPares (tail xs) -}

--b
{- entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                           then x : entrelazar xs []
                           else x : head ys : entrelazar xs (tail ys)
 -}

--a.
-- No es estructural, en el llamado recursivo usa el valor de xs.
--b.
-- Esta es legal:

--entrelazar' :: [a] -> [a] -> [a]

{- entrelazar' xs ys = foldr (\x rec ys -> if null ys
                              then x : rec 
                              else x : head ys : rec  (tail ys)) id xs -}



entrelazar1 :: [a] -> ([a] -> [a])
entrelazar1 = foldr (\x rec ys -> 
                    if null ys
                    then x : rec [] 
                    else x : head ys : rec (tail ys)
                    ) (const []) 


--6 a

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec ->
                       if x == e 
                       then xs
                       else x : rec ) [] 



--6 c

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> 
                           if  e > x 
                           then x : rec 
                           else e : x : xs) [e]


--7a


mapPares :: (a -> c -> b) -> [(a,c)] -> [b]
mapPares f xs = foldr (\x rec -> uncurry f x : rec) [] xs
-- o tambien map (\ x -> uncurry f x)
-- o mejor aun map (uncurry f)


--7b

armarPares:: [a] -> [b] -> [(a,b)]
armarPares = foldr (\x rec ys ->
                          if null ys
                          then []
                          else (x,head ys) : rec (tail ys)) (const []) 

--Insight: Si haces un foldr de algo que toma 2 parametros para resolver, entonces 
--existe una funcion parcialmente aplicada, asiq el caso base no puede ser solo el 
--operador de la lista vacia, sino uno que tome el argumento que le metan y ahi 
--devuelva la lista vacia.
--Y el orden de los parametros es siempre x (cabeza) , rec (paso recursivo), ys (lo
--que quieras)

--7c 

mapDoble:: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)


{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

Si xs = [x1, x2, x3] entonces:
foldr f z xs = f x1 (f x2 (f x3 z))

Equivalentemente con notaci´on infija:
foldr ⋆ z xs = x1 ⋆ (x2 ⋆ (x3 ⋆ z))
-}

armarPares':: [a] -> [b] -> [(a,b)]
armarPares' [] _ = []
armarPares' _ [] = []
armarPares' (x:xs) (y:ys) = (x,y) : armarPares' xs ys 


--8a
{- Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
todas de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones -}

sumaMat:: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\x rec ys -> zipWith (+) x (head ys) : rec (tail ys)) (const [])

--8b 
{- Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N. -}

{- trasponer:: [[Int]] -> [[Int]]
trasponer -} 


trasponer :: [[Int]] -> [[Int]]
trasponer m = foldr (\xs rec -> zipWith (:) xs rec) (replicate (length (head m)) []) m
--Esto esta copiado de honi, es increible la verdad.

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)


--9a
--data Nat = Cero | Succ Nat


foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z --WHY?????
foldNat f z n = f n (foldNat f z (n-1))


potencia :: Integer -> Integer -> Integer
potencia x n = foldNat (\n rec -> x * rec) 1 n

--10a.

{- Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de
una cantidad dada de elementos, a partir de un elemento inicial y de una función de incremento 
entre los elementosde la lista. Dicha función de incremento, dado un elemento de la lista, devuelve 
el elemento siguiente. -}


genLista :: a -> (a -> a) -> Integer -> [a]
genLista inicial incremento cant = foldNat (\x rec -> inicial : map incremento rec) [] (cant +1)

{- genListaRec:: a -> ((a -> a) -> (Integer -> [a]))
genListaRec ini f 0 = []
genListaRec ini f n = f ini : genListaRec (f ini) f (n-1)     -}


--10.b
{- Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo. -}


desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta a b = genLista a (+1) (b-a)


--11.

data Polinomio a = X 
    | Cte a 
    | Suma (Polinomio a) (Polinomio a) 
    | Prod (Polinomio a) (Polinomio a)
    deriving (Show)

foldPoli:: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fx fcte fsuma fprod poli =
    case poli of
        X           -> fx
        Cte a       -> fcte a
        Suma a b    -> fsuma (rec a) (rec b)
        Prod a b    -> fprod (rec a) (rec b)
    where rec = foldPoli fx fcte fsuma fprod

evaluar:: Num a => a -> Polinomio a -> a
evaluar a = foldPoli a id (+) (*)

--12a.

data AB a = Nil | Bin (AB a) a (AB a) 
    deriving (Show)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB fNil fBin ab =
    case ab of 
        Nil         -> fNil
        Bin i r d   -> fBin (rec i) r (rec d)
    where rec = foldAB fNil fBin

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB fNil fBin ab = 
    case ab of 
        Nil         -> fNil
        Bin i r d   -> fBin i r d (rec i) (rec d) 
    where rec = recAB fNil fBin


--12b.

esNil :: AB a -> Bool
esNil x = 
    case x of 
        Nil -> True
        _   -> False 

altura :: AB a -> Int
altura = foldAB 0 (\d r i -> 1 + max d i)

cantNodos :: AB a -> Int 
cantNodos = foldAB 0 (\d r i -> 1 + d + i)

--12c.

--Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
--Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión
--para un árbol que puede o no ser Nil.

mejorSegun1 :: (a -> a -> Bool) -> AB a -> a
mejorSegun1 pred (Bin izq r der) = foldAB r (\i r d -> if pred d r then (if pred d i then d else i) else (if pred r i then r else i)) (Bin izq r der)
 

--12.d 
{- Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
derecho. -}

esABB :: Ord a => AB a -> Bool
esABB (Bin i r d) = recAB True (\i r d recI recD -> (mayor r i) && (menor r d) && recI && recD) (Bin i r d)
    where mayor _ Nil = True
          mayor x ab  = x >= mejorSegun1 (>) ab
          menor _ Nil = True
          menor x ab  = x <  mejorSegun1 (<) ab




--13.a 

--i. Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo.

ramas:: AB a -> [[a]]
ramas= foldAB [] (\recI r recD -> if null recI && null recD then [[r]] else map (r:) (recI ++ recD))
{- Si llega a un Nil, devuelve la lista vacia, y el otro lado tmabien, tenes un nodo sin hijos, 
devuelvel el valor del nodo como [[r]], si solo uno de los 2 es Nil, tonces concatena [] con el
resultado que llegue, [] ++ xs = xs, asiq lo deja igual, y despues a todo lo que tenga xs le agrega 
la raiz actual. Si tenes 2 hijos con cosas, vas a tener algo como [[x]] y [[y]], asiq primero queres 
unificarlos en una lista, [[x]] ++ [[y]] = [[x],[y]], y despues le agregas el elemento actual de tu 
raiz, asi va subiendo la rama, map (r:) ([[x]] ++ [[y]]) = map (r:) ([[x],[y]]) que tonces queda 
=  [[r,x],[r,y]], es una maravilla. Puntos claves, ver como el foldAB empieza al final y va subiendo,
lo que te permite concatenar las listas y cuando haces el map (r:), se lo tas haciendo a todas las 
listas que son ramas de ese mismo nodo. Tambien el if Null, porq sino, cuando llega a Nil, solo 
devuelve muchas listas vacias y nunca agregas nada a la lista, map (r:) ([] ++ [] = []) = [], y
map f [] = [].
-}


cantHojas:: AB a -> Int
cantHojas = foldAB 0 (\recI r recD -> if (recI == 0 && recD == 0) then 1 else recI + recD)

espejo:: AB a -> AB a
espejo = foldAB Nil (\recI r recD -> Bin recD r recI)
--Pretty magico

--13.b
{- Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árboles, indica si éstos
tienen la misma forma, independientemente del contenido de sus nodos. Pista: usar evaluación parcial y
recordar el ejercicio 7. -}


mismoArbol :: (Eq a) => AB a -> AB a -> Bool
mismoArbol = recAB (const True) (\i r d recI recD abb -> case abb of 
                                     Nil        -> esNil (Bin i r d)
                                     Bin i rr d -> (rr == r) && (recI i) && (recD d)
                              ) 
                            
-- Soy un falopero que no lee el enunciado, no habia que ver si son iguales, habia que ver que tengan 
-- la misma forma en ramas y eso.... miss Morbol, sameree

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = recAB (const True) (\i r d recI recD abb -> case abb of 
                                     Nil        -> False -- Este es "arbol A tiene rama aca pero B no"
                                     Bin i _ d  -> (recI i) && (recD d)
                              ) 
-- Extra: No tiene necesidad de ser un recAB, solo que me daba pereza cambiarlo.

--14.a
--Se desea modelar en Haskell los árboles con información en las hojas (y sólo en ellas). Para esto introduciremos
--el siguiente tipo:

data AIH a = Hoja a | Bin1 (AIH a) (AIH a)
    deriving (Show)

{- Definir el esquema de recursión estructural foldAIH y dar su tipo. Por tratarse del primer esquema de
recursión que tenemos para este tipo, se permite usar recursión explícita -}

foldAIH:: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fBin1 ab = 
    case ab of
        Hoja a   -> fHoja a
        Bin1 i d -> fBin1 (rec i) (rec d)
    where rec = foldAIH fHoja fBin1 

--14.b

{- Escribir las funciones altura :: AIH a -> Integer y tamaño :: AIH a -> Integer.
Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas.
 -}

alturaAIH :: AIH a -> Integer -- Longitud camino mas largo hasta una hoja
alturaAIH = foldAIH (\a -> 1) (\recI recD -> 1 + max recI recD)

tamañoAIH :: AIH a -> Integer -- Cantidad de hojas
tamañoAIH = foldAIH (const 1) (\recI recD -> recI + recD)


--15.a

data RoseTree a = RoseTree a [RoseTree a] 
    deriving (Show)

--15.b

foldRT:: (a -> [b] -> b) -> RoseTree a -> b
foldRT fRT (RoseTree x xs) = fRT x (map (foldRT fRT) xs)

--15.c.1

hojasRT:: RoseTree a -> [a]
hojasRT = foldRT (\v recHijos -> 
    case recHijos of
        []     -> [v] -- Sino tiene hijos, lo meto en una lista
        (x:xs) -> concat recHijos) -- Concat, dada muchas listas en una lista, concatena todo en una sola
                                   -- concat [[1, 2, 3], [4, 5], [6], []] --> [1,2,3,4,5,6]

--15.c.2
-- distancias, que dado un RoseTree, devuelva las distancias de su raíz a cada una de sus hojas.

distancias:: RoseTree a -> [Integer]
distancias = foldRT (\_ recHijos -> 
    case recHijos of
        []     -> [1]
        (x:xs) -> map (+1) (concat recHijos))


union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = nub (xs ++ ys)


--15.c.3
{- altura, que devuelve la altura de un RoseTree (la cantidad de nodos de la rama más larga). Si el
RoseTree es una hoja, se considera que su altura es 1. -}

alturaRT:: RoseTree a -> Integer
alturaRT rs = maximum (distancias rs) -- Se siente tramposo(?)


-- Primer parcial segundo cuatri 2024, programacion funcional
{- Aclaración: en este ejercicio no está permitido utilizar recursión explícita, a menos que se indique lo contrario.

El siguiente tipo de datos sirve para representar un buffer con historia que permite escribir o leer en
cualquiera de sus posiciones (las posiciones tienen tipo Int). 
-El tipo del buffer es paramétrico en el tipo de los contenidos que se pueden guardar en él.
-Si se escribe dos veces en la misma posición, el nuevo contenido pisa al anterior (por simplicidad). 
-La lectura elimina el contenido leído. -}
{-
Por ejemplo, los buffers 
-b1 = Write 2 True Empty 
-b2 = Write 2 True (Write 2 False Empty) y
-b3 = Read 1 (Write 2 True (Write i False Empty))
tienen todos el valor True en la posición 2 y nada en las demás,
aunque tienen distinta historia y podrían distinguirse mirando el historial de operaciones realizadas.

Parece segun el b2 que la historia es ultima accion (accion anterior qcy), se lee de adentro hacia 
afuera pa la cronologia. 

b2 = Write 2 True (Write 2 False Empty)
b2 = [vacio,vacio,..] -> Write 2 False -> [vacio,False,..] -> Write 2 True -> [vacio,True,..]

Y importante que la lectura elimina el contenido:
b3 = Read 1 (Write 2 True (Write 1 False Empty))
b3 = [vacio,vacio,..] -> Write 1 False -> [False,vacio,..] -> Write 2 True -> [False,True,..] -> Read 1 -> [vacio,True,..] 

-}

data Buffer a = Empty | Write Int a (Buffer a) | Read Int (Buffer a)
    deriving (Show)
--a. Dar el tipo y definir la función foldBuffer y recBuffer, que implementan respectivamente 
-- los es- quemas de recursión estructural y primitiva para el tipo Buffer a. 
-- Solo en este inciso se permite usar recursión explícita.

foldBuff:: b -> (Int -> a -> b -> b) -> (Int -> b -> b) -> Buffer a -> b
foldBuff fEmpty fWrite fRead buff =
    case buff of 
        Empty       -> fEmpty
        Write i a x -> fWrite i a (rec x)
        Read i x    -> fRead i (rec x)  
    where rec = foldBuff fEmpty fWrite fRead

recBuff:: b -> (Int -> a -> Buffer a -> b -> b) -> (Int -> Buffer a -> b -> b) -> Buffer a -> b
recBuff fEmpty fWrite fRead buff =
    case buff of 
        Empty       -> fEmpty
        Write i a x -> fWrite i a x (rec x)
        Read i x    -> fRead i x (rec x)  
    where rec = recBuff fEmpty fWrite fRead

--b.
{- Definir la función posiciones cupadas:: Buffer a -> [Int], que lista las posiciones ocupadas en
un buffer (sin posiciones repetidas).
Por ejemplo: posicionesOcupadas buf [1, 2]. -}

posicionesOcupadas :: Buffer a -> [Int] 
posicionesOcupadas = foldBuff [] (\i _ recBuff -> if elem i recBuff
                                                  then recBuff
                                                  else i : recBuff) 
                                 (\i recBuff -> filter (i /=) recBuff) 

--c.
{- Definir la función contenido:: Int -> Buffer a -> Maybe a, que devuelva el contenido de una posición en
un buffer si hay algo en ella, y Nothing en caso contrario.
buf = Write 1 'a' $ Write 2 'b' $ Write 1 'c' $ Empty
contenido 1 buf          --> Just a
contenido (-2) buf       --> Nothing.
contenido 1 (Read 1 buf) --> Nothing.
-}

contenido:: Int -> Buffer a -> Maybe a --Leelo en otro momento a ver que onda.
contenido pos = foldBuff Nothing (\int v recBuff -> if int == pos then Just v else recBuff)
                                 (\int recBuff   -> if int == pos then Nothing else recBuff)

--d.

{- Definir la función puedeCompletarLecturas::Buffer a -> Bool, que indique si todas las lecturas
pueden completarse exitosamente (es decir, si cada vez que se intenta leer una posición, hay algo
escrito en ella).
Por ejemplo:

puedeCompletarLecturas (Read 1 Empty)        --> False.
puedeCompletarLecturas (Read 1 buf)          --> True.
puedeCompletarLecturas (Read 1 $ Read 1 buf) --> False.
 -}

puedeCompletarLecturas :: Buffer a -> Bool
puedeCompletarLecturas = recBuff True (\i v buff recBuff -> recBuff) 
                                      (\i buff recBuff -> elem i (posicionesOcupadas buff) && recBuff)


--e.
{- Definir la función deshacer::Buffer a -> Int -> Buffer a, que dados un buffer y un número
natural n (es decir, un Int que por contexto de uso no es negativo), deshaga las últimas n operaciones
del buffer, sacando los n constructores más externos para obtener un buffer como el original antes de
realizar dichas operaciones. Si se realizaron menos de n operaciones, el resultado debe quedar vacío.

Por ejemplo: deshacer 2 buf Write 1 ''c'" Empty. 
-}

deshacer::Buffer a -> Int -> Buffer a -- Leelo en otro momento.
deshacer = recBuff (const Empty) (\i v buff recBuff n -> if n == 0 then (Write i v buff) else recBuff (n-1)) 
                                 (\i buff recBuff n -> if n == 0 then (Read i buff) else recBuff (n-1))


-- Primer parcial primer cuatri 2024, programacion funcional
--El siguiente tipo de datos sirve para representar árboles ternarios:

data AT a = NilT | Tri a (AT a) (AT a) (AT a)

{- Definimos el siguiente árbol para los ejemplos:
at1 = Tri 1 (Tri 2 NilT NilT NilT) (Tri 3 (Tri 4 NilT NilT NilT) NilT NilT)
(Tri 5 NilT NilT NilT) -}


--a. Dar el tipo y definir la función foldAT que implementa el esquema de recursión estructural para el tipo
--AT a. Sólo en este inciso se permite usar recursión explícita.

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT fNil fTri ab =
    case ab of
        NilT        -> fNil
        Tri x a b c -> fTri x (rec a) (rec b) (rec c)
    where rec = foldAT fNil fTri  


{- 
b. Definir la función preorder :: AT a -> [a], que lista los nodos de un árbol ternario en el orden en
que aparecen: primero la raíz, después los nodos del subárbol izquierdo, luego los del medio y finalmente
los del derecho.
Por ejemplo: preorder at1 [1, 2, 3, 4, 5]. -}

preOrder :: AT a -> [a]
preOrder = foldAT [] (\r i m d -> [r] ++ i ++ m ++ d)

{- 
c. Definir la función mapAT :: (a -> b) -> AT a -> AT b, análoga a la función map para listas, pero
para árboles ternarios.

Por ejemplo: mapAT (+1) at1 = Tri 2 (Tri 3 NilT NilT NilT) (Tri 4 (Tri 5 NilT NilT) Nill
NilT) (Tri 6 NilT NilT NilT) -}

mapAT :: (a -> b) -> AT a -> AT b
mapAT f = foldAT NilT (\r i m d -> Tri (f r) i m d)

{- 
d. Definir la función nivel :: AT a -> Int -> [a], que devuelve la lista de nodos 
del nivel correspondiente del árbol, siendo 0 el nivel de la raíz.
Por ejemplo: nivel at1 1 [2, 3, 5]. -}

nivel :: AT a -> Int -> [a]
nivel = foldAT (const []) (\r i m d -> \n -> if n == 0 then [r] else (i (n-1)) ++ (m (n-1)) ++ (d (n-1)))


--Primer parcial primer cuatri 2024, programacion funcional

{- 
En este ejercicio vamos a modelar lógica proposicional en Haskell, de modo de poder construir fórmulas
proposicionales y evaluarlas bajo distintas valuaciones. -}

data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop
    deriving (Show)

type Valuacion = String -> Bool

{- 
Por ejemplo, la expresión: Y (Var "P?) (No (Imp (Var 'Q'') (Var "R''))) representa la proposición 
P y no ( Q -> R).

Las valuaciones se representan como funciones que a cada variable proposicional le asignan un valor booleano. 
Por ejemplo la valuación \x -> x == 'p' le asigna el valor verdadero a la variable P y falso a todas las
otras variables proposicionales.

a.Dar el tipo y definir las funciones foldProp y recProp, que implementan respectivamente los esquemas
de recursión estructural y primitiva para el tipo Prop. Solo en este inciso se permite usar recursión explícita.
 -}

foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop -> b
foldProp fVar fNot fAnd fOr fImp prop =
    case prop of 
        Var s   -> fVar s
        No x    -> fNot (rec x)
        Y x y   -> fAnd (rec x) (rec y)
        O x y   -> fOr (rec x) (rec y)
        Imp x y -> fImp (rec x) (rec y)
    where rec = foldProp fVar fNot fAnd fOr fImp


recProp :: (String -> b) -> (Prop -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> Prop -> b
recProp fVar fNot fAnd fOr fImp prop =
    case prop of 
        Var s   -> fVar s
        No x    -> fNot x (rec x)
        Y x y   -> fAnd x y (rec x) (rec y)
        O x y   -> fOr x y (rec x) (rec y)
        Imp x y -> fImp x y (rec x) (rec y)
    where rec = recProp fVar fNot fAnd fOr fImp

{- 
b.Definir la función variables :: Prop -> [String], que dada una fórmula devuelve la lista con todas
sus variables proposicionales en algún orden, sin elementos repetidos. -}

variables :: Prop -> [String]
variables = foldProp (\s -> [s]) 
                     (\x -> x) 
                     (\x y -> union x y) 
                     (\x y -> union x y)
                     (\x y -> union x y)


{- 
Definir la función evaluar :: Valuación -> Prop -> Bool, que indica si una fórmula es verdadera o
falsa para una valuación dada. -}

evaluar1 :: Valuacion -> Prop -> Bool
evaluar1  v = foldProp (\s -> v s) (\x -> not x) (\x y -> x && y) (\x y -> x || y) (\x y -> (not x) || y)

{- 
d. Definir la función estáEnFNN :: Prop -> Bool, que indica si una fórmula está en Forma Normal Negada.
Es decir, si no tiene implicaciones y la negación se aplica únicamente a variables y no a proposiciones más complejas.
Por ejemplo: Y (Var "P'") (No (Imp (Var "Q'") (Var "R*'))) no está en FNN,
y en cambio Y (Var ''P'') (Y (Var 'Q'') (No (Var ''R'"))) sí lo está. -}

--FNN <-> NO HAY "-->" y LA NEGACION SOLO TA SOBRE VARIABLES, NO ALGUNA PROP.

estaEnFNN :: Prop -> Bool
estaEnFNN = recProp (const True) 
                    (\prop x -> case prop of
                        Var _   -> True 
                        No _    -> False
                        Y _ _   -> False 
                        O _ _   -> False 
                        Imp _ _ -> False) 
                    (\prop1 prop2 x y -> x && y) 
                    (\prop1 prop2 x y -> x && y) 
                    (\prop1 prop2 x y -> False)





{- (<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 foo Linea d1

    where foo = ( \t rec -> case rec of
    Texto t' d -> Texto (t ++ t') d
    _ -> Texto t rec
)

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (\i' -> Linea (i + i'))
 -}
