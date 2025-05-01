--- ---
--- title: Práctica 1 - Programación funcional
--- author: Ernesto Ittig
--- date: 2025-03-27
--- ---
{-# LANGUAGE LambdaCase #-}

module Ejercicio.PLP.Practica01
where

import Data.List (nub)

--- ## Currificación y tipos

--- ### Ejercicio 1

--- 1. _¿Cuál es el tipo de cada función?_

max2 :: (Ord a) => (a, a) -> a
max2 (x, y)
  | x >= y = x
  | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x ^ 2 + y ^ 2)

subtract_ :: (Num a) => a -> a -> a
subtract_ = flip (-)

predecesor :: (Num a) => a -> a
predecesor = subtract_ 1

evaluarEnCero :: (Num a) => (a -> b) -> b
evaluarEnCero f = f 0

dosVeces :: (a -> a) -> a -> a
dosVeces f = f . f

flipAll :: [b -> a -> c] -> [a -> b -> c]
flipAll = map flip

flipRaro :: a -> (b -> a -> c) -> b -> c
flipRaro = flip flip

--- 2. _Indicar cuáles de las funciones anteriores no están currificadas. Para
---    cada una de ellas, definir la función currificada correspondiente_

max2' :: (Ord a) => a -> a -> a
max2' x y
  | x >= y = x
  | otherwise = y

normaVectorial' :: Float -> Float -> Float
normaVectorial' x y = sqrt (x ^ 2 + y ^ 2)

--- ### Ejercicio 2
--- 1. _Definir la función `curry`, que dada una función de dos argumentos,
---    devuelve su equivalente currificada._

curry_ :: ((a, b) -> c) -> a -> b -> c
curry_ f x y = f (x, y)

--- 2. _Definir la función `uncurry`, que dada una función currificada de dos
---    argumentos, devuelve su versión no currificada equivalente. Es la inversa
---    de la anterior._

uncurry_ :: (a -> b -> c) -> (a, b) -> c
uncurry_ f (x, y) = f x y

--- 3. _Se podría definir una función `curryN`, que tome una función de un
---    número arbitrario de argumentos y devuelva su versión currificada?_
---
---    No se puede con el sistema de tipos de Haskell.

--- ## Esquemas de recursión

--- ### Ejercicio 3

--- 1. _Redefinir usando foldr las funciones `sum`, `elem`, `(++)`, `filter` y
---   `map`._

sum_ :: (Num a) => [a] -> a
sum_ = foldr (+) 0

elem_ :: (Eq a) => a -> [a] -> Bool
elem_ x = foldr ((||) . (x ==)) False

(+++) :: [a] -> [a] -> [a]
(+++) = flip $ foldr (:)

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p = foldr (\x r -> if p x then x : r else r) []

map_ :: (a -> b) -> [a] -> [b]
map_ f = foldr ((:) . f) []

--- 2. _Definir la función `mejorSegún :: (a -> a -> Bool) -> [a] -> a`, que
---    devuelve el máximo elemento de la lista según una función de comparación,
---    utilizando `foldr1`. Por ejemplo, `maximum = mejorSegún (>)`._

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x y -> if p x y then x else y)

--- 3. _Definir la función `sumasParciales :: Num a => [a] -> [a]`, que dada una
---    lista de números devuelve otra de la misma longitud, que tiene en cada
---    posición la suma parcial de los elementos de la lista original desde la
---    cabeza hasta la posición actual. Por ejemplo,
---    `sumasParciales [1,4,-1,0,5] → [1,5,4,4,9]`._

sumasParciales :: Num a => [a] -> [a]
sumasParciales = scanl1 (+)

--- 4. _Definir la función `sumaAlt`, que realiza la suma alternada de los
---    elementos de una lista. Es decir, da como resultado: el primer elemento,
---    menos el segundo, más el tercero, menos el cuarto, etc. Usar `foldr`._

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

--- 5. _Hacer lo mismo que en el punto anterior, pero en sentido inverso (el
---    último elemento menos el anteúltimo, etc.). Pensar qué esquema de
---    recursión conviene usar en este caso._

sumaAlt' :: Num a => [a] -> a
sumaAlt' = foldl (-) 0

--- ### Ejercicio 4

--- 1. _Definir la función `permutaciones :: [a] -> [[a]]`, que dada una lista
---    devuelve todas sus permutaciones_

permutaciones :: [a] -> [[a]]
permutaciones = foldr (concatMap . intercambiar) [[]]
 where
  intercambiar v xs = [take n xs ++ [v] ++ drop n xs | n <- [0 .. (length xs)]]

--- 2. _Definir la función partes, que recibe una lista L y devuelve la lista de
---    todas las listas formadas por los mismos elementos de L, en su mismo
---    orden de aparición._

partes :: [a] -> [[a]]
partes = foldr (concatMap . (\x xs -> [xs, x : xs])) [[]]

--- 3. _Definir la función prefijos, que dada una lista, devuelve todos sus
---    prefijos._

prefijos :: [a] -> [[a]]
prefijos xs = [take n xs | n <- [0 .. (length xs)]]

--- 4. _Definir la función sublistas que, dada una lista, devuelve todas sus
---    sublistas (listas de elementos que aparecen consecutivos en la lista
---    original)._

sublistas :: [a] -> [[a]]
sublistas ls = [] : [take n . drop m $ ls | n <- [1 .. t], m <- [0 .. (t - n)]]
 where
  t = length ls

--- ### Ejercicio 5

--- _Considerar las siguientes funciones:_

--- _Indicar si la recursión utilizada en cada una de ellas es o no estructural.
--- Si lo es, reescribirla utilizando foldr. En caso contrario, explicar el
--- motivo._

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x : xs) =
  if null xs
    then [x]
    else x : elementosEnPosicionesPares (tail xs)

--- No es estructural, porque la función opera y decide sobre el valor de `xs`.

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x : xs) = \ys ->
  if null ys
    then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)

--- Es recursión estructural.

--- ### Ejercicio 6

--- _El siguiente esquema captura la recursión primitiva sobre listas._

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--- a) _Definir la función `sacarUna :: Eq a => a -> [a] -> [a]`, que dados un
---    elemento y una lista devuelve el resultado de eliminar de la lista la
---    primera aparición del elemento (si está presente)._

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna v = recr (\x xs rec -> if x == v then xs else x : rec) []

--- b) _Explicar por qué el esquema de recursión estructural (`foldr`) no es
---    adecuado para implementar la función `sacarUna` del punto anterior._

---    Cuando se encuentra la primera instancia del carácter a sacar, se debe
---    detener la recursión y devolver la cola de la lista. Para hacer eso se
---    debe tener acceso a la cola de la lista, que `foldr` no proporciona.

--- c) Definir la función `insertarOrdenado :: Ord a => a -> [a] -> [a]` que
---    inserta un elemento en una lista ordenada (de manera creciente), de
---    manera que se preserva el ordenamiento

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado v = recr (\x xs rec -> if x > v then v : x : xs else x : rec) []

--- ### Ejercicio 7

--- _Definir las siguientes funciones para trabajar sobre listas, y dar su tipo.
--- Todas ellas deben poder aplicarse a listas *finitas* e *infinitas*._

--- i) _`mapPares`, una versión de `map` que toma una función currificada de dos
---    argumentos y una lista de pares de valores, y devuelve la lista de
---    aplicaciones de la función a cada par._

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

--- ii) `armarPares`, que dadas dos listas arma una lista de pares que contiene,
---     en cada posición, el elemento correspondiente a esa posición en cada una
---     de las listas. Si una de las listas es más larga que la otra, ignorar
---     los elementos que sobran (el resultado tendrá la longitud de la lista
---     más corta). Esta función en Haskell se llama `zip`.

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr sig (const [])
 where
  sig x rec [] = []
  sig x rec (y : ys) = (x, y) : rec ys

--- iii) `mapDoble`, una variante de `mapPares`, que toma una función
---      currificada de dos argumentos y dos listas (de igual longitud), y
---      devuelve una lista de aplicaciones de la función a cada elemento
---      correspondiente de las dos listas. Esta función en Haskell se llama
---      `zipWith`.

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = (mapPares f .) . armarPares

--- ### Ejercicio 8

--- i) _Escribir la función `sumaMat`, que representa la suma de matrices, usando
---    `zipWith`. Representaremos una matriz como la lista de sus filas. Esto
---    quiere decir que cada matriz será una lista finita de listas finitas,
---    todas de la misma longitud, con elementos enteros. Recordamos que la suma
---    de matrices se define como la suma celda a celda. Asumir que las dos
---    matrices a sumar están bien formadas y tienen las mismas dimensiones._

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

--- ii) _Escribir la función `trasponer`, que, dada una matriz como las del ítem
---     $i$, devuelva su traspuesta. Es decir, en la posición $i, j$ del
---     resultado está el contenido de la posición $j, i$ de la matriz original.
---     Notar que si la entrada es una lista de $N$ listas, todas de longitud
---     $M$ , la salida debe tener $M$ listas, todas de longitud $N$._

transponer :: [[Int]] -> [[Int]]
transponer = foldr sacarP []
 where
  sacarP xs = zipWith (:) xs . fillN (length xs)
  fillN n xs = xs ++ replicate (n - length xs) []

--- ### Ejercicio 9

--- i) _Definir y dar el tipo del esquema de recursión `foldNat` sobre los
---    naturales. Utilizar el tipo `Integer` de Haskell (la función va a estar
---    definida sólo para los enteros mayores o iguales que 0)._

foldNat :: b -> (b -> b) -> Integer -> b
foldNat fCero fSuc n =
  case n of
    0 -> fCero
    n -> fSuc (foldNat fCero fSuc (n - 1))

--- ii) _Utilizando `foldNat`, definir la función potencia._

potencia :: Integer -> Integer -> Integer
potencia n = foldNat 1 (* n)

--- ### Ejercicio 10

--- i) _Definir la función `genLista :: a -> (a -> a) -> Integer -> [a]`, que genera
---    una lista de una cantidad dada de elementos, a partir de un elemento
---    inicial y de una función de incremento entre los elementos de la lista.
---    Dicha función de incremento, dado un elemento de la lista, devuelve el
---    elemento siguiente._

genLista :: a -> (a -> a) -> Integer -> [a]
genLista z f n = scanl (flip (const f)) z [1 .. n]

--- ii) _Usando `genLista`, definir la función `desdeHasta`, que dado un par de
---     números (el primero menor que el segundo), devuelve una lista de números
---     consecutivos desde el primero hasta el segundo._
desdeHasta :: (Integer, Integer) -> [Integer]
desdeHasta (x, y) = genLista x (+ 1) (y - x)

--- ### Ejercicio 11

--- _Definir el esquema de recursión estructural para el siguiente tipo:_

data Polinomio a
  = X
  | Cte a
  | Suma (Polinomio a) (Polinomio a)
  | Prod (Polinomio a) (Polinomio a)

--- _Luego usar el esquema definido para escribir la función
--- `evaluar :: Num a => a -> Polinomio a -> a` que, dado un número y un
--- polinomio, devuelve el resultado de evaluar el polinomio dado en el número
--- dado._

foldPol :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPol fX fCte fSuma fProd pol =
  case pol of
    X -> fX
    Cte v -> fCte v
    Suma p1 p2 -> fSuma (rec p1) (rec p2)
    Prod p1 p2 -> fProd (rec p1) (rec p2)
 where
  rec = foldPol fX fCte fSuma fProd

evaluar :: Num a => a -> Polinomio a -> a
evaluar x = foldPol x id (+) (*)

--- ### Ejercicio 12

--- _Considerar el siguiente tipo, que representa a los árboles binarios:_

data AB a = Nil | Bin (AB a) a (AB a)
  deriving (Show)

--- i) _Usando recursión explícita, definir los esquemas de recursión estructural
---    (foldAB) y primitiva (recAB), y dar sus tipos._

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB fNil fBin tree =
  case tree of
    Nil -> fNil
    Bin l x r -> fBin (rec l) x (rec r)
 where
  rec = foldAB fNil fBin

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB fNil fBin tree =
  case tree of
    Nil -> fNil
    Bin l x r -> fBin l x r (rec l) (rec r)
 where
  rec = recAB fNil fBin

--- ii) _Definir las funciones `esNil`, `altura` y `cantNodos`_

esNil :: AB a -> Bool
esNil = \case Nil -> True; _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\l _ v -> 1 + max l v)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\l _ v -> 1 + l + v)

--- iii) _Definir la función `mejorSegún :: (a -> a -> Bool) -> AB a -> a,`
---      análoga a la del ejercicio 3, para árboles. Se recomienda definir una
---      función auxiliar para comparar la raíz con un posible resultado de la
---      recursión para un árbol que puede o no ser `Nil`._

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB p t = case foldAB Nothing elegir t of Just x -> x
 where
  elegir l x r = max' (Just x) (max' l r)
  max' = maybe id (\x -> fmap $ \y -> if p x y then x else y)

--- iv) _Definir la función `esABB :: Ord a => AB a -> Bool` que chequea si un
---     árbol es un árbol binario de búsqueda. Recordar que, en un árbol binario
---     de búsqueda, el valor de un nodo es mayor o igual que los valores que
---     aparecen en el subárbol izquierdo y es estrictamente menor que los
---     valores que aparecen en el subárbol derecho._
esABB :: Ord a => AB a -> Bool
esABB =
  recAB
    True
    (\l x r recL recR -> recL && recR && mejor (>=) x l && mejor (<) x r)
 where
  mejor f x = \case Nil -> True; t -> f (mejorSegunAB f t) x

--- v) _Justificar la elección de los esquemas de recursión utilizados para los
---    tres puntos anteriores._

--- Usé recursión estructural siempre que pude. En `esABB` necesitaba poder
--- acceder a todos los hijos de un nodo para ver que se cumpla la condición. 👍

--- ### Ejercicio 13

--- _Dado el tipo `AB a` del ejercicio 12:_

--- i) _Definir las funciones `ramas` (caminos desde la raíz hasta las hojas),
---    `cantHojas` y `espejo`._

ramas :: AB a -> [[a]]
ramas =
  foldAB
    []
    ( \l x r -> case l ++ r of
        [] -> [[x]]
        xs -> map (x :) xs
    )

cantHojas :: AB a -> Integer
cantHojas = foldAB 0 (\l x r -> case l + r of 0 -> 1; v -> v)

espejo :: AB a -> AB a
espejo = foldAB Nil (\l x r -> Bin r x l)

--- ii) _Definir la función `mismaEstructura :: AB a -> AB b -> Bool` que, dados
---     dos árboles, indica si éstos tienen la misma forma, independientemente
---     del contenido de sus nodos. Pista: usar evaluación parcial y recordar el
---     ejercicio 7._

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura =
  foldAB
    esNil
    ( \l _ r -> \case
        Nil -> False
        Bin l' _ r' -> l l' && r r'
    )

--- ### Ejercicio 14

--- _Se desea modelar en Haskell los árboles con información en las hojas (y
--- sólo en ellas). Para esto introduciremos el siguiente tipo:_

data AIH a = Hoja a | Bin' (AIH a) (AIH a)
  deriving (Show)

--- a) _Definir el esquema de recursión estructural `foldAIH` y dar su tipo. Por
---    tratarse del primer esquema de recursión que tenemos para este tipo, se
---    permite usar recursión explícita._

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fBin = \case
  Hoja a -> fHoja a
  Bin' l r -> fBin (rec l) (rec r)
 where
  rec = foldAIH fHoja fBin

--- b) _Escribir las funciones `altura :: AIH a -> Integer` y `tamaño :: AIH a -> Integer`._
---    Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas.
alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (const 1) (((+ 1) .) . max)

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (+)

--- ### Ejercicio 15

--- i) _Definir el tipo `RoseTree` de árboles no vacíos, con una cantidad
---    indeterminada de hijos para cada nodo._

data RoseTree a = Rose a [RoseTree a]

--- ii) _Escribir el esquema de recursión estructural para `RoseTree`.
---     Importante escribir primero su tipo._

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose fRose (Rose x r) = fRose x (map (foldRose fRose) r)

--- iii) _Usando el esquema definido, escribir las siguientes funciones:_
---      a) _`hojas`, que dado un `RoseTree`, devuelva una lista con sus hojas
---         ordenadas de izquierda a derecha, según su aparición en el `RoseTree`_
---      b) _`distancias`, que dado un `RoseTree`, devuelva las distancias de su
---         raíz a cada una de sus hojas._
---      c) _`altura`, que devuelve la altura de un RoseTree (la cantidad de
---         nodos de la rama más larga). Si el `RoseTree` es una hoja, se
---         considera que su altura es 1._

hojas :: RoseTree a -> [a]
hojas = foldRose (\v -> \case [] -> [v]; rss -> concat rss)

distancias :: RoseTree a -> [Integer]
distancias = foldRose (const $ \case [] -> [1]; rss -> map (+ 1) . concat $ rss)

alturaRT :: RoseTree a -> Integer
alturaRT = foldRose (const $ \case [] -> 1; rss -> 1 + maximum rss)

--- ### Ejercicio 16

--- Se desea representar conjuntos mediante Hashing abierto (chain addressing).
--- El Hashing abierto consta de dos funciones: una función de Hash, que dado un
--- elemento devuelve un valor entero (el cual se espera que no se
--- repita con frecuencia), y una tabla de Hash, que dado un número entero devuelve
--- los elementos del conjunto a los que la función de Hash asignó dicho número (es
--- decir, la preimagen de la función de Hash para ese número).

--- Los representaremos en Haskell de la siguiente manera:

data HashSet a = Hash (a -> Integer) (Integer -> [a])

--- Por contexto de uso, vamos a suponer que la tabla de Hash es una función total,
--- que devuelve listas vacías para los números que no corresponden a elementos
--- del conjunto. Este es un invariante que deberá preservarse en todas las
--- funciones que devuelvan conjuntos.

--- _Definir las siguientes funciones:_

--- i) _`vacío :: (a -> Integer) -> HashSet a`, que devuelve un conjunto vacío
---    con la función de Hash indicada._

vacio :: (a -> Integer) -> HashSet a
vacio f = Hash f (return [])

--- ii) _`pertenece :: Eq a => a -> HashSet a -> Bool`, que indica si un elemento
---     pertenece a un conjunto. Es decir, si se encuentra en la lista obtenida
---     en la tabla de Hash para el número correspondiente a la función de Hash
---     del elemento._

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece x (Hash f g) = x `elem` (g . f) x

--- iii) _`agregar :: Eq a => a -> HashSet a -> HashSet a`, que agrega un
---      elemento a un conjunto. Si el elemento ya estaba en el conjunto, se debe
---      devolver el conjunto sin modificaciones._

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar x h@(Hash f g)
  | pertenece x h = h
  | otherwise = Hash f (\x' -> if x' == f x then x : g x' else g x')

--- iv) _`intersección :: Eq a => HashSet a -> HashSet a -> HashSet a` que,
---     dados dos conjuntos, devuelve un conjunto con la misma función de Hash
---     del primero y con los elementos que pertenecen a ambos conjuntos a la vez._

intersección :: Eq a => HashSet a -> HashSet a -> HashSet a
intersección (Hash f g1) (Hash _ g2) = Hash f (\x -> nub $ g1 x ++ g2 x)

--- v) _`foldr1` (no relacionada con los conjuntos). Dar el tipo y definir la
---    función foldr1 para listas sin usar recursión explícita, recurriendo a
---    alguno de los esquemas de recursión conocidos. Se recomienda usar la
---    función `error :: String -> a` para el caso de la lista vacía._

foldr1_ :: (a -> a -> a) -> [a] -> a
foldr1_ _ [] = error "lista vacía :("
foldr1_ f xs = foldr f (last xs) xs

--- ## Generación infinita

--- ### Ejercicio 17

--- _¿Cuál es el valor de esta expresión?_

[1, 3] =
  [x | x <- [1 .. 3], y <- [x .. 3], (x + y) `mod` 3 == 0]

--- ### Ejercicio 18

paresDeNat :: [(Int, Int)]
paresDeNat = [(i, k - i) | k <- [0 ..], i <- [0 .. k]]

--- ### Ejercicio 19

--- _Una tripla pitagórica es una tripla (a, b, c) de enteros positivos tal que
--- $a^2 + b^2 = c^2$. La siguiente expresión intenta ser una definición de una
--- lista (infinita) de triplas pitagóricas:_

pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas =
  [(a, b, c) | a <- [1 ..], b <- [1 ..], c <- [1 ..], a ^ 2 + b ^ 2 == c ^ 2]

--- _Explicar por qué esta definición no es útil. Dar una definición mejor._

--- Esta definición no es útil porque genera todas las (infinitas) triplas que
--- empiezan con 1 primero, y por lo tanto nunca se llegarán a generar otras. Se
--- podría definir la función de acuerdo a $c$ usando la desigualdad triangular:

pitagoricas' :: [(Integer, Integer, Integer)]
pitagoricas' =
  [(a, b, c) | c <- [1 ..], a <- [1 .. c], b <- [1 .. c], a ^ 2 + b ^ 2 == c ^ 2]

--- ### Ejercicio 20

--- Escribir la función `listasQueSuman :: Int -> [[Int]]` que, dado un número
--- natural $n$, devuelve todas las listas de enteros positivos (es decir,
--- mayores o iguales que 1) cuya suma sea $n$. Para este ejercicio se permite
--- usar recursión explícita.

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = [x : l | x <- [1 .. n], l <- listasQueSuman (n - x)]

--- ### Ejercicio 21

--- Definir en Haskell una lista que contenga todas las listas finitas de
--- enteros positivos (esto es, con elementos mayores o iguales que 1).

listasFinitasDeNat :: [[Int]]
listasFinitasDeNat = [l | i <- [0 ..], l <- listasQueSuman i]

--- ### Ejercicio 23

--- _Dado el tipo de datos `AIH a` definido en el ejercicio 14:_

--- a) _Definir la lista (infinita) de todos los AIH cuyas hojas tienen tipo ().
---    Se recomienda definir una función auxiliar. Para este ejercicio se
---    permite utilizar recursión explícita._

todosAIH :: [AIH ()]
todosAIH = [a | i <- [1 ..], a <- conAltura i]
 where
  conAltura 1 = [Hoja ()]
  conAltura 2 = [Bin' (Hoja ()) (Hoja ())]
  conAltura n =
    [Bin' l r | l <- conAltura (n - 1), r <- conAltura (n - 1)]
      ++ [Bin' (Hoja ()) t | t <- conAltura (n - 1)]
      ++ [Bin' t (Hoja ()) | t <- conAltura (n - 1)]

--- b) _Explicar por qué la recursión utilizada en el punto a) no es estructural._

--- qsy (?)