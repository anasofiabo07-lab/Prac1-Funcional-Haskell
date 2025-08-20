-- orderDesc.hs
-- Ordena una lista de números flotantes en orden descendente
--
-- Input:
--   - Lista de Floats.
-- Output:
--   - Lista ordenada de mayor a menor.
--
-- Nota: Se implementa un "quicksort" adaptado.

orderDesc :: [Float] -> [Float]
orderDesc [] = [] -- Caso base: lista vacía → lista vacía
orderDesc (x:xs) =
  -- Divide en dos grupos: mayores y menores/iguales
  orderDesc [y | y <- xs, y > x] ++ [x] ++ orderDesc [y | y <- xs, y <= x]

-- Ejemplo:
-- orderDesc [1,25,5,-4]
-- Paso 1: Elige 1 como pivote.
-- Paso 2: Mayores a 1 → [25,5], menores o iguales → [-4].
-- Paso 3: Repite el proceso hasta ordenar.
-- Resultado: [25,5,1,-4]
