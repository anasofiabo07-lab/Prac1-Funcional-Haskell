-- logFunc.hs
-- Aproxima ln(1+x) usando N términos de la serie de Taylor.
--
-- Serie (válida para |x| < 1):
-- ln(1+x) = Σ [ ((-1)^(n+1)) * x^n / n ] para n=1 hasta N-1
--
-- Input:
--   - x: valor en el que se evalúa (debe cumplir |x| < 1).
--   - n: número de términos.
-- Output:
--   - Aproximación de ln(1+x).

logFunc :: Float -> Int -> Float
logFunc x n = sum [ ((-1)^(k+1)) * (x^k) / fromIntegral k | k <- [1..(n-1)] ]

-- Ejemplo:
-- logFunc 0.5 10
-- Resultado ≈ 0.4055