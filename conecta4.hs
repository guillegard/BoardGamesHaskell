import           Data.List  (foldl', maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ord   (comparing)

--Definimos a los posibles jugadores
data Jugador = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)

--para cada espacio puede tener nada, X, O
muestraEspacio :: Maybe Jugador -> Char
muestraEspacio Nothing  = ' '
muestraEspacio (Just X) = 'X'
muestraEspacio (Just O) = 'O'

-- definicion del tablero
data Tablero = Tablero
    { filasTablero    :: Int
    , columnasTabler :: Int
    , cuadranteTablero   :: Map (Int, Int) Jugador
    }

--Imprime el tablero
instance Show Tablero where
    show tablero@(Tablero filas valorPorColumna _) = unlines $
        [ concat [show i | i <- [0 .. valorPorColumna - 1]]
        ] ++
        [ [muestraEspacio (encontrar fila columna tablero) | columna <- [0 .. valorPorColumna - 1]]
        | fila <- [0 .. filas - 1]
        ]

--Revisa si el campo es vacio
campoVacio :: Int -> Int -> Tablero
campoVacio filas valorPorColumna = Tablero filas valorPorColumna M.empty

--encientra cierto lugar en el tablero
encontrar :: Int -> Int -> Tablero -> Maybe Jugador
encontrar fila columna = M.lookup (fila, columna) . cuadranteTablero

--Agrega una ficha en una columna
agegar :: Int -> Jugador -> Tablero -> Tablero
agegar columna tile tablero@(Tablero filas valorPorColumna tiles)
    | columna < 0 || columna >= valorPorColumna = tablero
    | fila < 0                         = tablero
    | otherwise                       =
        Tablero filas valorPorColumna $ M.insert (fila, columna) tile tiles
  where
    fila = parteSuperior columna tablero - 1

--Revisa la parte superior del tablero para ver si esta lleno
parteSuperior :: Int -> Tablero -> Int
parteSuperior columna tablero@(Tablero filas _ _) = ir 0
  where
    ir fila
        | fila > filas                      = filas
        | encontrar fila columna tablero /= Nothing = fila
        | otherwise                       = ir (fila + 1)

--Revisa las concurrencias de fichasen diversas direcciones
conexiones :: Int -> Tablero -> [[(Int, Int)]]
conexiones tamano (Tablero filas valorPorColumna _) =
    [ [(r, c + i) | i <- is]
    | r <- [0 .. filas - 1], c <- [0 .. valorPorColumna - tamano]
    ] ++

    [ [(r + i, c) | i <- is]
    | r <- [0 .. filas - tamano], c <- [0 .. valorPorColumna - 1]
    ] ++

    [ [(r + i, c + i) | i <- is]
    | r <- [0 .. filas - tamano], c <- [0 .. valorPorColumna - tamano]
    ] ++

    [ [(r + i, c - i) | i <- is]
    | r <- [0 .. filas - tamano], c <- [tamano - 1 .. valorPorColumna - 1]
    ]
  where
    is = [0 .. tamano - 1]

--Cuebta el numero de concurrencias
cuenta :: [Maybe Jugador] -> Maybe (Jugador, Int)
cuenta tiles = case catMaybes tiles of
    []                  -> Nothing
    (x : xs)
        | all (== x) xs -> Just (x, length xs + 1)
        | otherwise     -> Nothing


--Obitnene las concurrencias de concexiones
frecuencia :: Int -> Tablero -> Jugador -> Int -> Int
frecuencia tamano tablero =
    let map' = foldl' step M.empty $ conexiones tamano tablero
    in \p l -> fromMaybe 0 $ M.lookup (p, l) map'
  where
    step freqs connection =
        let tiles = map (\(r, c) -> encontrar r c tablero) connection
        in case cuenta tiles of
            Just c  -> M.insertWith' (+) c 1 freqs
            Nothing -> freqs

--Identifica si algun jugador gano
ganador :: Int -> Tablero -> Maybe Jugador
ganador tamano tablero = listToMaybe
    [ p
    | p <- [minBound .. maxBound]
    , frecuencia' p tamano > 0
    ]
  where
    frecuencia' = frecuencia tamano tablero

--Cuenta el nmarcado actual
marcador :: Int -> Tablero -> Jugador -> Int
marcador tamano tablero me = sum
    [ actual * marcador' * frecuencia' p l
    | p <- [minBound .. maxBound]
    , l <- [1 .. tamano]
    , let actual   = if p == me then 1 else -1
    , let marcador' = l ^ (2 * l)
    ]
  where
    frecuencia' = frecuencia tamano tablero

--Decide en donde poner una ficha para la computadora
ai :: Int -> Tablero -> Jugador -> Int
ai tamano tablero me = fst $ maximumBy (comparing snd)
    [ (col, marcador tamano (agegar col me tablero) me)
    | col <- [0 .. columnasTabler tablero - 1]
    ]

--Controlador principal, el usuario jugara con las fichas "X"
main :: IO ()
main = ir (cycle jugadores) $ campoVacio 7 9
  where
    jugadores :: [(Jugador, Int -> Tablero -> IO Int)]
    jugadores =
        [ (X, \_ _ -> readLn)
        , (O, \tamano tablero -> return $ ai tamano tablero O)
        ]

    ir []             _     = return ()
    ir ((p, pf) : ps) tablero = do
        putStr $ show tablero
        case ganador 4 tablero of
            Just w  -> putStrLn $ "Jugador " ++ show w ++ " gana!"
            Nothing -> do
                putStrLn $ "Jugador " ++ show p ++ " es su turno, selecciona una columna para poner una ficha"
                n <- pf 4 tablero
                ir ps $ agegar n p tablero
