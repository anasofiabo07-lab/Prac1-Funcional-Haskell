-- remData.hs
-- Descripción:
--   Esta función elimina de una lista todos los valores que no estén dentro
--   del intervalo [a, b]. Es decir, solo devuelve los números que cumplen
--   con estar entre los dos límites.
--
-- Input:
--   - xs: lista de enteros.
--   - a: valor mínimo permitido.
--   - b: valor máximo permitido.
-- Output:
--   - Una lista con solo los elementos que cumplen a <= x <= b.

remData :: [Int] -> Int -> Int -> [Int]
-- La comprensión de listas filtra solo los elementos que cumplen la condición.
remData xs a b = [x | x <- xs, x >= a, x <= b]

-- Ejemplo de prueba:
-- remData [1, 25, 5, -4] 0 5
-- Paso 1: Toma la lista [1,25,5,-4]
-- Paso 2: Revisa cada elemento y lo compara con el rango [0,5].
-- Paso 3: Solo cumplen 1 y 5.
-- Resultado: [1,5]
