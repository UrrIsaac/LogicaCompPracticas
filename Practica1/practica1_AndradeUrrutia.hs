
{-
    Incluir comentarios explicando cada función
    No usar funciones de biblioteca que resuelvan directamente los ejercicios
-}

{-
    Nombres : 
        - Andrade Castañeda Angel
        - Urrutia Alfaro Isaac Arturo
    
    Fecha de entrega : 17 de Febero de 2026
-}

-- Espacio para actividad 1


-- Espacio para actividad 2



-- EJERCICIO 3. Máximo común divisor de dos números.
--      Descripción : calcula el maximo común divisor de dos numeros, basado en el algoritmo de euclides.
mcd :: Int -> Int -> Int
mcd a b
  | b == 0    = miAbs a     -- Como 0 es divisible por todo número, el máximo de la dupla es b.
  | otherwise = mcd b (a `mod` b) -- Hace la recursion basada en el algoritmo de euclides.

-- == Función auxiliar para no usar 'abs' de haskell.
miAbs :: Int -> Int
miAbs n
  | n < 0 = -n  -- Si es menor que cero cambia el signo.
  | otherwise = n


-- EJERCICIO 4. Reversa de lista.
--      Descripción : retorna la reversa de una lista de manera recursiva.
reversa :: [Int] -> [Int]
reversa [] = [] -- Caso base, la reversa de la vacia es ella misma.
reversa (x:xs) = reversa xs ++ [x] -- De manera recursiva deja que la cabeza ahora sea la cola.


-- EJERCICIO 5. Máximo de una lista.
--      Descripción : en base a una lista de números regresa el elemento máximo.
maximo :: [Int] -> Int
maximo [x] = x      -- Caso base, un sólo elemento.
maximo (x:xs) = miMax x (maximo xs)     --comparo la cabeza con el maximo del resto de la lista.

-- == Función auxiliar, para no utilizar el 'max' que ya tiene haskell.
miMax :: Int -> Int -> Int
miMax a b 
    | a <= b = b    -- Sólo compara.
    | otherwise = a


-- EJERCICIO 6. Filtrar lista por medio de números pares.
--      Descripción : con base a una lista regresa los elementos pares que tiene.
pares :: [Int] -> [Int]
pares [] = [] -- Caso base, como no hay elementos a comparar regresa vacía.
pares (x:xs)
    | esPar x = x : pares xs -- Si la cabeza es par la añado al resultado y comparo lo demás.
    | otherwise = pares xs -- Si la cabeza no es par sólo comparo lo demás.

-- == Función auxiliar para ver si un número es par.
esPar :: Int -> Bool
esPar n
    | n `mod` 2 == 0 = True -- Con el modulo digo si algo es par o no.
    | otherwise = False


-- EJERCICIO 7. Contar ocurrencias en lista
--      Descripción : con base a una lista y un número 'n', checa la lista y regresa el número de veces que aparece 'n'.
contar :: Int -> [Int] -> Int
contar n [] = 0 -- Caso base, como en la lista no hay elementos a comparar, hay 0 coincidencias.
contar n (x:xs)
    | n == x = 1 + contar n xs -- Si la cabeza es una coincidencia sumo uno y comparo lo demás.
    | otherwise = contar n xs -- Si la cabeza no es coincidencia comparo lo demás.


-- Espacio para actividad 8


-- Espacio para actividad 9


-- Espacio para actividad 10