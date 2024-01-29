-- Adrián Gómez Lamuedra Grupo DG

-- Definición de los tipos pedidos

-- EXPRESIONES

--Expresion aritmética
data ExpArit = Numero Integer| Var String | Suma ExpArit ExpArit | Resta ExpArit ExpArit | Mult ExpArit ExpArit deriving Show

-- Expresion booleana
data ExpBool = T| F | Igual ExpArit ExpArit | Menor ExpArit ExpArit | Mayor ExpArit ExpArit | MenorIgual ExpArit ExpArit | MayorIgual ExpArit ExpArit | Neg ExpBool | Con ExpBool ExpBool | Dis ExpBool ExpBool deriving Show


--INSTRUCCIONES
data Instruccion = Asignacion String ExpArit | Cond ExpBool Programa Programa | While ExpBool Programa deriving Show

--PROGRAMAS (una lista de instrucciones)
type Programa = [Instruccion]

--ESTADOS (lista de tuplas (variable, valor))
type Estado = [(String, Integer)]


--FUNCIONES AUXILIARES 

--Funcion para obtener el valor de una variable a partir del estado
obtenerValor :: String -> Estado -> Integer
obtenerValor var [] = error ("Variable " ++ var ++ " no encontrada")
obtenerValor var ((v, valor):resto)
  | var == v  = valor
  | otherwise = obtenerValor var resto
-- Buscamos la variable en el estado y si la encontramos devolvemos su valor, si no un error

--Funcion para modificar el valor de una variable y por tanto el estado
modificarValor :: String -> Integer -> Estado -> Estado
modificarValor var nuevoValor estado = (var, nuevoValor) : filter (\(v, _) -> v /= var) estado
-- Añadimos una tupla nueva de la variable con su nuevo valor y eliminamos la tupla de la variable con el valor anterior en caso de haberla

--Funciones para evaluar las expresiones

evExpArit :: ExpArit -> Estado -> Integer
evExpArit (Numero n) _ = n -- Evaluar un entero es devolver el mismo numero entero
evExpArit (Var var) estado = obtenerValor var estado -- Evaluar una  variable es devolver su valor
evExpArit (Suma e1 e2) estado = evExpArit e1 estado + evExpArit e2 estado -- Una suma de expresiones requiere evaluar recursivamente las expresiones de entrada y sumarlas hasta llegar a un unico numero o variable 
evExpArit (Resta e1 e2) estado = evExpArit e1 estado - evExpArit e2 estado -- Analogo a la suma
evExpArit (Mult e1 e2) estado = evExpArit e1 estado * evExpArit e2 estado -- Analogo a la suma 

evExpBool :: ExpBool -> Estado -> Bool
evExpBool  T _ = True -- Casos base o expresiones booleanas constantes 
evExpBool  F _ = False
evExpBool  (Igual e1 e2) estado = evExpArit  e1 estado == evExpArit  e2 estado -- Para las comparaciones de expresiones aritmeticas tenemos que evaluarlas llamando a evExpArit
evExpBool  (Menor e1 e2) estado = evExpArit  e1 estado < evExpArit  e2 estado
evExpBool  (Mayor e1 e2) estado = evExpArit  e1 estado > evExpArit  e2 estado
evExpBool  (MenorIgual e1 e2) estado = evExpArit  e1 estado <= evExpArit  e2 estado
evExpBool  (MayorIgual e1 e2) estado = evExpArit e1 estado >= evExpArit e2 estado
evExpBool  (Neg expB) estado = not (evExpBool expB estado) -- Para las operaciones logicas llamamos recursivamente a evExpBool hasta llegar a un caso base (True o False)
evExpBool  (Con expB1 expB2) estado = evExpBool expB1 estado && evExpBool expB2 estado
evExpBool  (Dis expB1 expB2) estado = evExpBool expB1 estado || evExpBool expB2 estado


--FUNCION PRINCIPAL

ejecuta :: Programa -> Estado -> Integer
ejecuta [] estado = obtenerValor "R" estado  -- No quedan mas instrucciones, el resultado final está en la variable R

ejecuta (Asignacion var expA : resto) estado = ejecuta resto (modificarValor var (evExpArit expA estado) estado)
-- Asignamos la evaluacion de la expresion a var y ejecutamos el resto de instrucciones

ejecuta (Cond expB p1 p2 : resto) estado = if evExpBool expB estado then ejecuta (p1 ++ resto) estado else ejecuta (p2 ++ resto) estado
-- Si se cumple la condicion ejecutamos el bloque de instrucciones p1 y seguimos con el programa, si se cumple la segunda, ejecutamos p2 y seguimos con el programa

ejecuta (While expB p : resto) estado = if evExpBool expB estado then ejecuta (p ++ [While expB p] ++ resto) estado else ejecuta resto estado
-- Ejecutamos el bloque p hasta que la condicion se deje de cumplir añadiendola a la secuencia de instrucciones continuamente, cuando se deja de cumplir la condicion seguimos con el resto


-- REPRESENTACIONES OBLIGATORIAS

factorial :: Programa
factorial = [Asignacion "Y" (Var "X"), Asignacion "R" (Numero 1), While (Menor (Numero 0) (Var "Y")) [Asignacion "R" (Mult (Var "R") (Var "Y")), Asignacion "Y" (Resta (Var "Y") (Numero 1))]]

s0 :: Estado
s0 = [("X", 3)]
