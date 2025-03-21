

data Sensacion = Frio | Calor
data Forma = Circ Float | Rect Float Float 

{- 
1. Dar el tipo y describir el comportamiento de las siguientes funciones del módulo Prelude de Haskell:


null: null :: Foldable t => t a -> Bool
Devuelve true si una estructura esta vacia

head: head :: GHC.Stack.Types.HasCallStack => [a] -> a
Saca el primer elemento de una lista, si es una vacia, explota.

tail: tail :: GHC.Stack.Types.HasCallSatck => [a] -> [a]
Devuelve la lista sin el primer elemento

init: init :: GHC.Stack.Types.HasCallStack => [a] -> [a]
Devuelve la lista sin el ultimo elem

last: last::GHC.Stack.Types.HasCallStack => [a] -> a
Saca el ultimo elemento de una lista    

take: take :: Int -> [a] -> [a]
Devuelve la lista con la cantidad de con la misma cantidad 
de elementos que entre de param

drop: drop :: Int -> [a] -> [a]
Devuelve lo que hay en la lista despues de llegar al param 
int.

(++): (++) :: [a] -> [a] -> [a]
Concatena listas

concat: (Prelude.concat) :: Foldable t => t [a] -> [a]
Ni idea

concat: (GHC.List.concat) :: [[a]] -> [a]
Concatena todas las listas en una sola

reverse: reverse :: [a] -> [a]
Da vuetla una lista

elem: GHC.List.elem :: Eq a => a -> [a] -> Bool
Te dice si un elemento pertenece a una lista
-}

-- 2. Definir las funciones: 

--Que dado un número devuelve su valor absoluto
valorAbsoluto :: Float -> Float
valorAbsoluto x | x <= 0 = -x
                | otherwise = x

--Que dado un número que representa un año, indica si el mismo es bisiesto.
bisiesto :: Int -> Bool
bisiesto x = (mod x 4 == 0 && mod x 100 /= 0) || mod x 400 == 0

--Definida únicamente para enteros positivos, que computa el factorial.
factorial :: Int -> Int
factorial 0 = 1
factorial x = factorial (x-1) * x

--Que dado un entero positivo devuelve la cantidad de divisores primos.

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = cdpAUX x x 0

cdpAUX :: Int -> Int -> Int -> Int
cdpAUX _ 1 res = res
cdpAUX x cont res | mod x cont == 0 && esPrimo cont = cdpAUX x (cont-1) (res + 1)
                  | otherwise = cdpAUX x (cont -1 ) res
           
esPrimo :: Int -> Bool 
esPrimo x = esPrimoAUX x (x-1)

esPrimoAUX :: Int -> Int -> Bool
esPrimoAUX 1 _ = False
esPrimoAUX 2 1 = True
esPrimoAUX _ 2 = True
esPrimoAUX x cont =  mod x cont /= 0 && esPrimoAUX x (cont-1)


-- 3. Usando Maybe y Either definidos como:
--data Maybe a = Nothing | Just a
--data Either a b = Left a | Right b

-- a. Definir la funcion inverso :: Float -> Maybe Float que dado un numero devuelve su 
-- inverso multiplicativo, si esta definido, sino, nothing

inverso :: Float -> Maybe Float
inverso 0 = Nothing 
inverso x = Just (1/x)

-- b. Definir la función aEntero :: Either Int Bool → Int que convierte a entero una expresión que puede ser
-- booleana o entera. En el caso de los booleanos, el entero que corresponde es 0 para False y 1 para True

aEntero :: Either Int Bool -> Int 
aEntero (Left x)  = x               -- Importantisimo los parentesis, sino no lo toma
aEntero (Right x) | not x = 0
                  | otherwise = 1

-- 4. Defini lo siguiente: 

-- a. limpiar :: String → String → String, que elimina todas las apariciones de cualquier carácter de la primera
-- cadena en la segunda. Por ejemplo, limpiar ‘‘susto’’ ‘‘puerta’’ evalúa a ‘‘pera’’. Nota: String es un
-- renombre de [Char]. La notación ‘‘hola’’ es equivalente a [‘h’,‘o’,‘l’,‘a’] y a ‘h’:‘o’:‘l’:‘a’:[].

--type String = [Char] asi podes ponerle alias a algun tipo


limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x:xs) ys = limpiar xs (quitarApariciones x ys)

quitarApariciones :: Char -> String -> String
quitarApariciones _ [] = []
quitarApariciones letra (x:xs) | letra == x = quitarApariciones letra xs
                               | otherwise = x : quitarApariciones letra xs


-- b. difPromedio :: [Float] → [Float] que dada una lista de números devuelve la diferencia de cada uno con el
-- promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1]

{- difPromedio :: [Float] -> [Float]
difPromedio xs = difPromedioAUX xs xs

difPromedioAUX :: [Float] -> [Float] -> [Float]
difPromedioAUX [] _ = []
difPromedioAUX (x:xs) ys = let prom = promedio ys
                in (x - prom ys) : difPromedioAUX xs ys -}
    



promedio :: [Float] -> Float
promedio xs = sum xs / long xs

long :: [a] -> Float
long [] = 0
long (x:xs) = 1 + long xs

-- c. todosIguales :: [Int] → Bool que indica si una lista de enteros tiene todos sus elementos iguales.

todosIguales :: [Int] -> Bool 
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs) 


--5_ Dado el modelo de AB:

data AB a = Nil | Bin (AB a) a (AB a)

-- En este caso, el arbol binario tiene 2 posibles valores, Nil de que no hay nada, y Bin que es un 
-- con sus respectivos arboles cada uno y su valor.

-- a. vacioAB :: AB a → Bool que indica si un árbol es vacío (i.e. no tiene nodos).

vacioAB :: AB a -> Bool 
vacioAB Nil = True
vacioAB (Bin x y z) = False


-- b. negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación
-- de cada uno de los nodos.

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin x y z) = Bin (negacionAB x) (not y) (negacionAB z)

inOrder :: AB a -> [a]
inOrder Nil = []
inOrder (Bin x y z) = inOrder x ++ [y] ++ inOrder z


-- c. productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol.

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin x y z) = y * (productoAB x) * productoAB z
