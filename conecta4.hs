import           Data.List  (foldl', maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ord   (comparing)

data Jugador = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)

muestraEspacio :: Maybe Jugador -> Char
muestraEspacio Nothing  = ' '
muestraEspacio (Just X) = 'X'
muestraEspacio (Just O) = 'O'

data Field = Field
    { fieldRows    :: Int
    , fieldColumns :: Int
    , fieldTiles   :: Map (Int, Int) Jugador
    }

instance Show Field where
    show field@(Field filas pCol _) = unlines $
        [ concat [show i ++ " "| i <- [0 .. pCol - 1]]
        ] ++
        [ [muestraEspacio (get fila columna field) | columna <- [0 .. pCol - 1]]
        | fila <- [0 .. filas - 1]
        ]

campoVacio :: Int -> Int -> Field
campoVacio filas pCol = Field filas pCol M.empty


get :: Int -> Int -> Field -> Maybe Jugador
get fila columna = M.lookup (fila, columna) . fieldTiles


push :: Int -> Jugador -> Field -> Field
push columna tile field@(Field filas pCol tiles)
    | columna < 0 || columna >= pCol = field
    | fila < 0                         = field
    | otherwise                       =
        Field filas pCol $ M.insert (fila, columna) tile tiles
  where
    fila = parteSuperior columna field - 1

parteSuperior :: Int -> Field -> Int
parteSuperior columna field@(Field filas _ _) = go 0
  where
    go fila
        | fila > filas                      = filas
        | get fila columna field /= Nothing = fila
        | otherwise                       = go (fila + 1)

conexiones :: Int -> Field -> [[(Int, Int)]]
conexiones tamano (Field filas pCol _) =

    [ [(r, c + i) | i <- is]
    | r <- [0 .. filas - 1], c <- [0 .. pCol - tamano]
    ] ++

    [ [(r + i, c) | i <- is]
    | r <- [0 .. filas - tamano], c <- [0 .. pCol - 1]
    ] ++

    [ [(r + i, c + i) | i <- is]
    | r <- [0 .. filas - tamano], c <- [0 .. pCol - tamano]
    ] ++

    [ [(r + i, c - i) | i <- is]
    | r <- [0 .. filas - tamano], c <- [tamano - 1 .. pCol - 1]
    ]
  where
    is = [0 .. tamano - 1]


cuenta :: [Maybe Jugador] -> Maybe (Jugador, Int)
cuenta tiles = case catMaybes tiles of
    []                  -> Nothing
    (x : xs)
        | all (== x) xs -> Just (x, length xs + 1)
        | otherwise     -> Nothing


frecuencia :: Int -> Field -> Jugador -> Int -> Int
frecuencia tamano field =
    let map' = foldl' step M.empty $ conexiones tamano field
    in \p l -> fromMaybe 0 $ M.lookup (p, l) map'
  where
    step freqs connection =
        let tiles = map (\(r, c) -> get r c field) connection
        in case cuenta tiles of
            Just c  -> M.insertWith' (+) c 1 freqs
            Nothing -> freqs

ganador :: Int -> Field -> Maybe Jugador
ganador tamano field = listToMaybe
    [ p
    | p <- [minBound .. maxBound]
    , frecuencia' p tamano > 0
    ]
  where
    frecuencia' = frecuencia tamano field

marcador :: Int -> Field -> Jugador -> Int
marcador tamano field me = sum
    [ sign * marcador' * frecuencia' p l
    | p <- [minBound .. maxBound]
    , l <- [1 .. tamano]
    , let sign   = if p == me then 1 else -1
    , let marcador' = l ^ (2 * l)
    ]
  where
    frecuencia' = frecuencia tamano field


ai :: Int -> Field -> Jugador -> Int
ai tamano field me = fst $ maximumBy (comparing snd)
    [ (col, marcador tamano (push col me field) me)
    | col <- [0 .. fieldColumns field - 1]
    ]

main :: IO ()
main = go (cycle jugadores) $ campoVacio 7 9
  where
    jugadores :: [(Jugador, Int -> Field -> IO Int)]
    jugadores =
        [ (X, \_ _ -> readLn)
        , (O, \tamano field -> return $ ai tamano field O)
        ]

    go []             _     = return ()
    go ((p, pf) : ps) field = do
        putStr $ show field
        case ganador 4 field of
            Just w  -> putStrLn $ "Jugador " ++ show w ++ " gana!"
            Nothing -> do
                putStrLn $ "Jugador " ++ show p ++ " es su turno!"
                n <- pf 4 field
                go ps $ push n p field
