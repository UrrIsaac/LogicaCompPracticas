{-
    Nombres : 
        - Andrade Castañeda Angel
        - Urrutia Alfaro Isaac Arturo
    
    Fecha de entrega : 17 de Febero de 2026
-}

-- EJERCICIO 1. Fibonnaci
--     Descripción: Definir una función que dado un natural n, regrese el n-ésimo elemento en la secuencia de fibonnaci
fibonacci:: Integer -> Integer
fibonacci 0 = 0    -- Caso base, si el lugar es 0 se regresa 0
fibonacci 1 = 1    -- Caso base, si el lugar es 1 se regresa 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)    -- Rescursion sumando los dos lugares anteriores

-- EJERCICIO 2. Módulo n
--    Descripción: Definir una función que recibe dos naturales n,m y regresa m %n.
modulo:: Int -> Int -> Int
modulo n m
 | m < n = m    --Si el dividendo es menor que el divisor regresamos el dividendo
 | otherwise = modulo n (m-n)    --En otro caso sacamos el modulo del divisor con la resta del dividendo menos el divisor

-- EJERCICIO 3. Máximo común divisor de dos números.
--      Descripción : calcula el maximo común divisor de dos numeros, basado en el algoritmo de euclides.
mcd :: Int -> Int -> Int
mcd a b
  | b == 0    = miAbs a     -- Como 0 es divisible por todo número, el máximo de la dupla es b.
  | otherwise = mcd b (b `modulo` a) -- Hace la recursion basada en el algoritmo de euclides.


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
    | 2 `modulo` n == 0 = True -- Con el modulo digo si algo es par o no.
    | otherwise = False


-- EJERCICIO 7. Contar ocurrencias en lista
--      Descripción : con base a una lista y un número 'n', checa la lista y regresa el número de veces que aparece 'n'.
contar :: Int -> [Int] -> Int
contar n [] = 0 -- Caso base, como en la lista no hay elementos a comparar, hay 0 coincidencias.
contar n (x:xs)
    | n == x = 1 + contar n xs -- Si la cabeza es una coincidencia sumo uno y comparo lo demás.
    | otherwise = contar n xs -- Si la cabeza no es coincidencia comparo lo demás.


-- EJERCICIO 8. Definición de números naturales
data Nat = Z | S Nat deriving Show    --Se agrego deriving Show para poder escribir en la terminal el resultado del ejercicio 10


-- EJERCICIO 9. Conversión a entero
--    Descripción : Definir una función que convierta un valor de tipo Nat a entero.
convertir:: Nat -> Integer
convertir Z = 0    --Caso base, Z es el 0 en los naturales
convertir (S n) = 1 + convertir n    --Sumamos uno por cada "S" que encontremos y de forma recusiva analizamos n


-- EJERCICIO 10. Multiplicación sobre Nat
--    Descripción: Definir la multiplicación de dos números naturales definidos con el tipo anterior, no utilizar la función anterior y realizar la suma definida en Haskell.
suma :: Nat -> Nat -> Nat    -- Definimos la suma en los naturales
suma Z n = n    --Caso base, si sumamos cualquier natural con "Z" nos regresa el natural 
suma (S m) n = S (suma m n)    -- De forma recursiva cada que encontremos el sucesor (S) de un natural sumado con otro natural regresamos el sucesor de la suma de ambos naturales

mult :: Nat -> Nat -> Nat    --Definimos la mutiplicacion
mult Z (S n) = Z    --Caso base, si multiplicamos por Z nos regresa Z
mult (S n) m = suma m (mult n m)    --De forma recursiva si encontramos la multiplicacion del sucesor de un natural con otro natural entonces aplicamos la funcion suma a el segundo natural con la multiplicacion de ambos naturales
