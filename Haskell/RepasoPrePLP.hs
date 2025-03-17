

esImpar :: Integral a => a -> Bool
esImpar x = mod x 2 == 0

boomYbang :: [Integer] -> [String]
boomYbang xs = [if x > 9 then "BANG!" else "BOOM!" |x <- xs, esImpar x]

longitud :: [Integer] -> Integer
longitud xs = longAUX xs 0

longAUX :: [Integer] -> Integer -> Integer
longAUX [] y     = y
longAUX (_:xs) y = longAUX xs (y + 1)

sumatoria :: [Integer] -> Integer 
sumatoria xs = sumatoriaAUX xs 0

sumatoriaAUX :: [Integer] -> Integer -> Integer
sumatoriaAUX [] y = y
sumatoriaAUX (x:xs) y = sumatoriaAUX xs (x+y)

longitud' xs = sumatoria [1| _ <- xs]

triangulosPerimetroDe :: [(Integer,Integer,Integer)] -> Integer -> [(Integer,Integer,Integer)]
triangulosPerimetroDe xs y = [ (a,b,c) | (a,b,c) <- xs , a*a + b*b == c*c , a+b+c == y ]

absoluto :: (Integral a) => a -> a
absoluto a | a < 0 = a * (-1)
           | otherwise = a


maximoAbsoluto :: (Integral a) => a -> a -> a
maximoAbsoluto x y | absoluto x > absoluto y = x
                   | otherwise = y


esMult :: Integer -> Integer -> Integer
esMult x y | mod x y == 0 = x
           | otherwise = 0

sumSoloMult:: (Integer, Integer,Integer) ->Integer ->Integer
sumSoloMult (x,y,z) n = esMult x n + esMult y n + esMult z n


medioFact :: Integer -> Integer
medioFact x | x <= 0 = 1
            | otherwise = x * medioFact (x - 2)

sumaDigitos :: Integer -> Integer
sumaDigitos x = sumaDigitosAUX x longitud x 0

sumaDigitosAUX :: Integer -> Integer -> Integer -> Integer
sumaDigitosAUX x long res = 

