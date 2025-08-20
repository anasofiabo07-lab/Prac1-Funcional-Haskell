-- dct.hs
-- Calcula la Transformada Discreta del Coseno (DCT) de una lista de valores.
--
-- Fórmula:
-- X(k) = a(k) * Σ [ x(n) * cos( (n+0.5)*π*k / N ) ]
-- donde:
--   a(k) = sqrt(1/N)   si k = 0
--   a(k) = sqrt(2/N)   si k > 0
--
-- Input:
--   - Lista de Floats.
-- Output:
--   - Lista de coeficientes DCT.

dct :: [Float] -> [Float]
dct xs = [ coef k | k <- [0..(n-1)] ]
  where
    n = length xs
    coef k = a k * sum [ xs!!m * cos(((fromIntegral m + 0.5) * pi * fromIntegral k) / fromIntegral n) | m <- [0..(n-1)] ]
    a k = if k == 0 then sqrt (1 / fromIntegral n) else sqrt (2 / fromIntegral n)

-- Ejemplo:
-- dct [1,2,3,4,5,6,7,8,9,10]
-- Resultado aproximado:
-- [17.3925, -9.0249, 0, -0.9667, 0, -0.3162, 0, -0.1279, 0, -0.0359]