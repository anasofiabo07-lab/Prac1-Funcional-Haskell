-- expFunc.hs
-- Aproxima e^x usando N términos de la serie de Taylor
--
-- Serie:
-- e^x = 1 + x + x^2/2! + x^3/3! + ... + x^(N-1)/(N-1)!
--
-- Input:
--   - x: valor a evaluar.
--   - n: número de términos.
-- Output:
--   - Aproximación del valor de e^x.

expFunc :: Float -> Int -> Float
expFunc x n = sum [ (x^k) / fromIntegral (factorial k) | k <- [0..(n-1)] ]
-- La lista genera todos los términos de la serie hasta N-1.
-- "fromIntegral" convierte el factorial (Int) a Float para evitar errores.

-- Función auxiliar: factorial
factorial :: Int -> Int
factorial 0 = 1
factorial k = k * factorial (k-1)

-- Ejemplo:
-- expFunc 1 10
-- Resultado: 2.7182817 (aproximación a e)
