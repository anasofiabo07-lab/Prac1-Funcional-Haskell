-- errorPercent.hs
-- Calcula el error porcentual entre un valor real y uno aproximado.
--
-- Fórmula:
-- error = |(real - approx) / real| * 100
--
-- Input:
--   - real: valor exacto (de Haskell).
--   - approx: valor aproximado (nuestra función).
-- Output:
--   - Error porcentual.

errorPercent :: Float -> Float -> Float
errorPercent real approx = abs ((real - approx) / real) * 100

-- Ejemplo:
errorPercent (exp 1) (expFunc 1 10)
Resultado: ≈ 0.00001 %