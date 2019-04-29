 {- 
    López Soto Ramses Antonio
    31531997-4
                              -}

-- Ejercicio 1
promedio3 :: Float -> Float -> Float -> Float
promedio3 a b c =  (a + b + c) / 3

-- Ejercicio 2
sumaMonedas :: Float -> Float -> Float -> Float -> Float -> Float
sumaMonedas a b c d e
        | a == 0 = (b * 1) + (c * 2) + (d * 5) + (e * 10)
        | b == 0 = (a * 0.50) + (c * 2) + (d * 5) + (e * 10)
        | c == 0 = (a * 0.50) + (b * 1) + (d * 5) + (e * 10)
        | d == 0 = (a * 0.50) + (b * 1) + (c * 2) + (e * 10)
        | e == 0 = (a * 0.50) + (b * 1) + (c * 2) + (d * 5)
        | a /= 0 && b /= 0 && c /= 0 && d /= 0 && e /= 0 = (a * 0.50) + (b * 1) + (c * 2) + (d * 5) + (e * 10)
        | otherwise = 0

-- Ejercicio 3
volumenEsfera :: Float -> Float
volumenEsfera r = (4 / 3) * pi * r ^ 3

-- Ejercicio 4
areaCoronaCircular :: Float -> Float -> Float
areaCoronaCircular r1 r2 = (pi * r2 ^ 2) - (pi * r1 ^ 2)

-- Ejercicio 5
ultimaCifra :: Int -> Int
ultimaCifra n = mod n 10

-- Ejercicio 6 
numeroMayor :: Int -> Int -> Int
numeroMayor n m 
        | n > m     = (n * 10) + m
        | otherwise = (m * 10) + n

-- Ejercicio 7
areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c = let s = (a + b + c) / 2
                  in sqrt(s * (s - a) * (s - b) * (s - c))

-- Ejercicio 8
suma2 :: Int -> Int -> Int -> Bool
suma2 a b c
        | (a + b) == c = True
        | (a + c) == b = True
        | (b + c) == a = True
        | otherwise    = False

-- Ejercicio 9
menor10 :: Int -> Int -> Int -> Bool
menor10 a b c
        | a == (b + 10) = True
        | a == (c + 10) = True
        | b == (a + 10) = True
        | b == (c + 10) = True
        | c == (a + 10) = True
        | c == (b + 10) = True
        | otherwise     = False

-- Ejercicio 10
compartenDigito :: Int -> Int -> Bool
compartenDigito a b
        | (div a 10) == (div b 10) = True
        | (mod a 10) == (mod b 10) = True
        | (div a 10) == (mod b 10) = True
        | (mod a 10) == (div b 10) = True
        | otherwise = False