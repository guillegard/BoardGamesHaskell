import Data.Array

-- Definicion de los tipos para un tablero

-- Numero en una posicion
type Numero = Int

-- Posicion es una tupla de eneros
type Posicion = (Int, Int)

--Tablero es un arreglo de posiciones y numeros.
type Tablero = Array Posicion Numero

--Variable que representa un tablero de sudoku
--Lo genera a partir del ejemplo de tablero
tableroSudoku :: Tablero
tableroSudoku = array ((0, 0), (8, 8)) $ creaTablero ejemplo

-- Ejemplo de como escribir el sudoku
ejemplo :: [[Numero]]
ejemplo = [[5, 3, 0,  0, 7, 0,  0, 0, 0],
           [6, 0, 0,  1, 9, 5,  0, 0, 0],
           [0, 9, 8,  0, 0, 0,  0, 6, 0],

           [8, 0, 0,  0, 6, 0,  0, 0, 3],
           [4, 0, 0,  8, 0, 3,  0, 0, 1],
           [7, 0, 0,  0, 2, 0,  0, 0, 6],

           [0, 6, 0,  0, 0, 0,  2, 8, 0],
           [0, 0, 0,  4, 1, 9,  0, 0, 5],
           [0, 0, 0,  0, 8, 0,  0, 7, 0]]

--Resuelve encuentra el primer tablero completado
resuelve :: Tablero -> Maybe Tablero
resuelve = encontrado . soluciones

--Busca las posibles soluciones de un tablero
soluciones :: Tablero -> [Tablero]
soluciones b = soluciones' (posicionEn0 b) b
  where
    soluciones' :: [Posicion] -> Tablero -> [Tablero]
    soluciones' []     b = [b]
    soluciones' (x:xs) b = concatMap (soluciones' xs) candidateTableros
      where
        candidateNumeros  = [m | m <- [1..9], posibleNumero m x b]
        candidateTableros = map (\m -> copiaDeNumero m x b) candidateNumeros

--determina si en una posicion no hay valor asignado 
posicionEn0 :: Tablero -> [Posicion]
posicionEn0 b = [(fila, columna) | fila <- [0..8], columna <- [0..8], b ! (fila, columna) == 0]

--Obtinene un posible numero si es que no hay cierto numero en la fila, columnaumna o cuadrante
posibleNumero :: Numero -> Posicion -> Tablero -> Bool
posibleNumero m (fila, columna) b = noEnCuadrante && noEnFila && noEnColuna
  where
    noEnCuadrante = notElem m $ b `marcasEnCuadrante` (fila, columna)
    noEnFila = notElem m $ b `marcasEnFila` fila
    noEnColuna = notElem m $ b `marcasEnColumna` columna

--Copia el numero
copiaDeNumero :: Numero -> Posicion -> Tablero -> Tablero
copiaDeNumero mark (fila, columna) b = b // [((fila, columna), mark)]

--busca concurrencias en el cuadrante
marcasEnCuadrante :: Tablero -> Posicion -> [Numero]
b `marcasEnCuadrante` (fila, columna) = [b ! loc | loc <- locations]
  where
    fila' = (fila `div` 3) * 3
    columna' = (columna `div` 3) * 3
    locations = range((fila', columna'), (fila' + 2, columna' + 2))

--Busca concurrencias en la fila actual
marcasEnFila :: Tablero -> Int -> [Numero]
b `marcasEnFila` fila = [b ! loc | loc <- range((fila, 0), (fila, 8))]

--Busca concurrencias en la columna actual
marcasEnColumna ::  Tablero -> Int -> [Numero]
b `marcasEnColumna` columna = [b ! loc | loc <- range((0, columna), (8, columna))]

--Crea el tablero de sudoku a partir de la entrada como el ejemplo
creaTablero :: [[Numero]] -> [(Posicion, Numero)]
creaTablero = concatMap creaFila . zip [0..8]
  where
    creaFila :: (Int, [Numero]) -> [((Int, Int), Numero)]
    creaFila (fila, marks) = creaColumna fila $ zip [0..8] marks

    creaColumna :: Int -> [(Int, Numero)] -> [((Int, Int), Numero)]
    creaColumna fila columnas = map (\(columna, m) -> ((fila, columna), m)) columnas

--Determina si se ha encontrado un tablero solucion
encontrado :: [a] -> Maybe a
encontrado []     = Nothing
encontrado (x:xs) = Just x

--Se imprime el arreglo solucion
printTablero :: Maybe Tablero -> IO ()
printTablero Nothing  = putStrLn "Sin solucion"
printTablero (Just b) = mapM_ putStrLn [show $ b `marcasEnFila` fila | fila <- [0..8]]

main = do
    let solucion = resuelve tableroSudoku
    printTablero solucion