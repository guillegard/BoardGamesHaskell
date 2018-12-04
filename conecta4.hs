import           Data.List  (foldl', maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ord   (comparing)

data Jugador = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)

titulo :: Maybe Jugador -> Char
titulo Nothing  = ' '
titulo (Just X) = 'X'
titulo (Just O) = 'O'

data Tablero = Tablero
    { filasTablero    :: Int
    , columnasTablero :: Int
    , espaciosTablero   :: Map (Int, Int) Jugador
    }

instance Show Tablero where
    show tablero@(Tablero filas columnas _) = unlines $
        [ concat [show i ++ " "| i <- [0 .. columnas - 1]]
        ] ++
        [ [titulo (get fila column tablero) | column <- [0 .. columnas - 1]]
        | fila <- [0 .. filas - 1]
        ]

tableroVacio :: Int -> Int -> Tablero
tableroVacio filas columnas = Tablero filas columnas M.empty

get :: Int -> Int -> Tablero -> Maybe Jugador
get fila column = M.lookup (fila, columna) . espaciosTablero

push :: Int -> Jugador -> Tablero -> Tablero
push columna tile tablero@(Tablero filas columnas espacios)
    | columna < 0 || columna >= columnas = tablero
    | fila < 0                         = tablero
    | otherwise                       =
        Tablero filas columnas $ M.insert (fila, columna) tile espacios
  where
    fila = parteSuperior columna tablero - 1

parteSuperior :: Int -> Tablero -> Int
parteSuperior columna tablero@(Tablero filas _ _) = go 0
  where
    go fila
        | fila > filas                      = filas
        | get fila columna tablero /= Nothing = fila
        | otherwise                       = go (fila + 1)

connections :: Int -> Tablero -> [[(Int, Int)]]
connections tamano (Tablero filas columnas _) =
    [ [(r, c + i) | i <- is]
    | r <- [0 .. filas - 1], c <- [0 .. columnas - tamano]
    ] ++

    [ [(r + i, c) | i <- is]
    | r <- [0 .. filas - tamano], c <- [0 .. columnas - 1]
    ] ++

    [ [(r + i, c + i) | i <- is]
    | r <- [0 .. filas - tamano], c <- [0 .. columnas - tamano]
    ] ++

    [ [(r + i, c - i) | i <- is]
    | r <- [0 .. filas - tamano], c <- [tamano - 1 .. columnas - 1]
    ]
  where
    is = [0 .. tamano - 1]

cuenta :: [Maybe Jugador] -> Maybe (Jugador, Int)
cuenta espacios = case catMaybes espacios of
    []                  -> Nothing
    (x : xs)
        | all (== x) xs -> Just (x, length xs + 1)
        | otherwise     -> Nothing

frecuencia :: Int -> Tablero -> Jugador -> Int -> Int
frecuencia tamano tablero =
    let map' = foldl' step M.empty $ connections tamano tablero
    in \p l -> fromMaybe 0 $ M.lookup (p, l) map'
  where
    step freqs connection =
        let espacios = map (\(r, c) -> get r c tablero) connection
        in case cuenta espacios of
            Just c  -> M.insertWith' (+) c 1 freqs
            Nothing -> freqs

ganador :: Int -> Tablero -> Maybe Jugador
ganador tamano tablero = listToMaybe
    [ p
    | p <- [masBajo .. tope]
    , frecuencia' p tamano > 0
    ]
  where
    frecuencia' = frecuencia tamano tablero

punto :: Int -> Tablero -> Jugador -> Int
punto tamano tablero me = sum
    [ sign * punto' * frecuencia' p l
    | p <- [masBajo .. tope]
    , l <- [1 .. tamano]
    , let sign   = if p == me then 1 else -1
    , let punto' = l ^ (2 * l)
    ]
  where
    frecuencia' = frecuencia tamano tablero

ai :: Int -> Tablero -> Jugador -> Int
ai tamano tablero me = fst $ maximumBy (comparing snd)
    [ (col, punto tamano (push col me tablero) me)
    | col <- [0 .. columnasTablero tablero - 1]
    ]

main :: IO ()
main = go (cycle players) $ tableroVacio 7 9
  where
    players :: [(Jugador, Int -> Tablero -> IO Int)]
    players =
        [ (X, \_ _ -> readLn)
        , (O, \tamano tablero -> return $ ai tamano tablero O)
        ]

    go []             _     = return ()
    go ((p, pf) : ps) tablero = do
        putStr $ show tablero
        case ganador 4 tablero of
            Just w  -> putStrLn $ "Jugador " ++ show w ++ " ganó!"
            Nothing -> do
                putStrLn $ "Jugador " ++ show p ++ " es su turno!"
                n <- pf 4 tablero
                go ps $ push n p tablero
