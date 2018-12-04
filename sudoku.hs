import Data.Array

main = do
    let solución = resuelve tableroSudoku
    printTablero solución

type Marcado = Int

type Posicion = (Int, Int)

type Tablero = Array Posicion Marcado

tableroSudoku :: Tablero
tableroSudoku = array ((0, 0), (8, 8)) $ verTablero ejemplo

-- Ejemplo de como escribir el sudoku
ejemplo :: [[Marcado]]
ejemplo = [[5, 3, 0,  0, 7, 0,  0, 0, 0],
                 [6, 0, 0,  1, 9, 5,  0, 0, 0],
                 [0, 9, 8,  0, 0, 0,  0, 6, 0],

                 [8, 0, 0,  0, 6, 0,  0, 0, 3],
                 [4, 0, 0,  8, 0, 3,  0, 0, 1],
                 [7, 0, 0,  0, 2, 0,  0, 0, 6],

                 [0, 6, 0,  0, 0, 0,  2, 8, 0],
                 [0, 0, 0,  4, 1, 9,  0, 0, 5],
                 [0, 0, 0,  0, 8, 0,  0, 7, 0]]

resuelve :: Tablero -> Maybe Tablero
resuelve = encontrado . solucións

solucións :: Tablero -> [Tablero]
solucións b = solucións' (emptyPosicions b) b
  where
    solucións' :: [Posicion] -> Tablero -> [Tablero]
    solucións' []     b = [b]
    solucións' (x:xs) b = concatMap (solucións' xs) candidateTableros
      where
        candidateMarcados  = [m | m <- [1..9], isPossibleMarcado m x b]
        candidateTableros = map (\m -> copyWithMarcado m x b) candidateMarcados

emptyPosicions :: Tablero -> [Posicion]
emptyPosicions b = [(row, col) | row <- [0..8], col <- [0..8], b ! (row, col) == 0]

isPossibleMarcado :: Marcado -> Posicion -> Tablero -> Bool
isPossibleMarcado m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m $ b `marcasEnFila` row
    notInColumn = notElem m $ b `marcasEnColumna` col
    notInBox    = notElem m $ b `marcasEnCuadrado` (row, col)

copyWithMarcado :: Marcado -> Posicion -> Tablero -> Tablero
copyWithMarcado mark (row, col) b = b // [((row, col), mark)]

marcasEnFila :: Tablero -> Int -> [Marcado]
b `marcasEnFila` row = [b ! loc | loc <- range((row, 0), (row, 8))]

marcasEnColumna ::  Tablero -> Int -> [Marcado]
b `marcasEnColumna` col = [b ! loc | loc <- range((0, col), (8, col))]

marcasEnCuadrado :: Tablero -> Posicion -> [Marcado]
b `marcasEnCuadrado` (row, col) = [b ! loc | loc <- locations]
  where
    row' = (row `div` 3) * 3
    col' = (col `div` 3) * 3
    locations = range((row', col'), (row' + 2, col' + 2))

verTablero :: [[Marcado]] -> [(Posicion, Marcado)]
verTablero = concatMap verFila . zip [0..8]
  where
    verFila :: (Int, [Marcado]) -> [((Int, Int), Marcado)]
    verFila (row, marks) = verCol row $ zip [0..8] marks

    verCol :: Int -> [(Int, Marcado)] -> [((Int, Int), Marcado)]
    verCol row cols = map (\(col, m) -> ((row, col), m)) cols

encontrado :: [a] -> Maybe a
encontrado []     = Nothing
encontrado (x:xs) = Just x

printTablero :: Maybe Tablero -> IO ()
printTablero Nothing  = putStrLn "Sin solucion"
printTablero (Just b) = mapM_ putStrLn [show $ b `marcasEnFila` row | row <- [0..8]]
