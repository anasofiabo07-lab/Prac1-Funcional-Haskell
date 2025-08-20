-- cosFunc.hs
-- Aproxima cos(x) usando N términos de la serie de Taylor.
--
-- Serie:
-- cos(x) = Σ [ (-1)^n * x^(2n) / (2n)! ] para n=0 hasta N-1
--
-- Input:
--   - x: ángulo en radianes.
--   - n: número de términos.
-- Output:
--   - Aproximación de cos(x).

cosFunc :: Float -> Int -> Float
cosFunc x n = sum [ ((-1)^k * (x^(2*k))) / fromIntegral (factorial (2*k)) | k <- [0..(n-1)] ]

factorial :: Int -> Int
factorial 0 = 1
factorial k = k * factorial (k-1)

-- Ejemplo:
-- cosFunc 1.5 6
-- Resultado ≈ 0.0707