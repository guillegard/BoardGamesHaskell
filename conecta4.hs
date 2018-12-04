import           Data.List  (foldl', maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ord   (comparing)

data Player = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)

muestraEspacio :: Maybe Player -> Char
muestraEspacio Nothing  = ' '
muestraEspacio (Just X) = 'X'
muestraEspacio (Just O) = 'O'

data Field = Field
    { fieldRows    :: Int
    , fieldColumns :: Int
    , fieldTiles   :: Map (Int, Int) Player
    }

instance Show Field where
    show field@(Field rows columns _) = unlines $
        [ concat [show i ++ " "| i <- [0 .. columns - 1]]
        ] ++
        [ [muestraEspacio (get row column field) | column <- [0 .. columns - 1]]
        | row <- [0 .. rows - 1]
        ]

campoVacio :: Int -> Int -> Field
campoVacio rows columns = Field rows columns M.empty


get :: Int -> Int -> Field -> Maybe Player
get row column = M.lookup (row, column) . fieldTiles


push :: Int -> Player -> Field -> Field
push column tile field@(Field rows columns tiles)
    | column < 0 || column >= columns = field
    | row < 0                         = field
    | otherwise                       =
        Field rows columns $ M.insert (row, column) tile tiles
  where
    row = parteSuperior column field - 1

parteSuperior :: Int -> Field -> Int
parteSuperior column field@(Field rows _ _) = go 0
  where
    go row
        | row > rows                      = rows
        | get row column field /= Nothing = row
        | otherwise                       = go (row + 1)

conexiones :: Int -> Field -> [[(Int, Int)]]
conexiones len (Field rows columns _) =

    [ [(r, c + i) | i <- is]
    | r <- [0 .. rows - 1], c <- [0 .. columns - len]
    ] ++

    [ [(r + i, c) | i <- is]
    | r <- [0 .. rows - len], c <- [0 .. columns - 1]
    ] ++

    [ [(r + i, c + i) | i <- is]
    | r <- [0 .. rows - len], c <- [0 .. columns - len]
    ] ++

    [ [(r + i, c - i) | i <- is]
    | r <- [0 .. rows - len], c <- [len - 1 .. columns - 1]
    ]
  where
    is = [0 .. len - 1]


cuenta :: [Maybe Player] -> Maybe (Player, Int)
cuenta tiles = case catMaybes tiles of
    []                  -> Nothing
    (x : xs)
        | all (== x) xs -> Just (x, length xs + 1)
        | otherwise     -> Nothing


frecuencia :: Int -> Field -> Player -> Int -> Int
frecuencia len field =
    let map' = foldl' step M.empty $ conexiones len field
    in \p l -> fromMaybe 0 $ M.lookup (p, l) map'
  where
    step freqs connection =
        let tiles = map (\(r, c) -> get r c field) connection
        in case cuenta tiles of
            Just c  -> M.insertWith' (+) c 1 freqs
            Nothing -> freqs

ganador :: Int -> Field -> Maybe Player
ganador len field = listToMaybe
    [ p
    | p <- [minBound .. maxBound]
    , frecuencia' p len > 0
    ]
  where
    frecuencia' = frecuencia len field

marcador :: Int -> Field -> Player -> Int
marcador len field me = sum
    [ sign * marcador' * frecuencia' p l
    | p <- [minBound .. maxBound]
    , l <- [1 .. len]
    , let sign   = if p == me then 1 else -1
    , let marcador' = l ^ (2 * l)
    ]
  where
    frecuencia' = frecuencia len field


ai :: Int -> Field -> Player -> Int
ai len field me = fst $ maximumBy (comparing snd)
    [ (col, marcador len (push col me field) me)
    | col <- [0 .. fieldColumns field - 1]
    ]

main :: IO ()
main = go (cycle jugadores) $ campoVacio 7 9
  where
    jugadores :: [(Player, Int -> Field -> IO Int)]
    jugadores =
        [ (X, \_ _ -> readLn)
        , (O, \len field -> return $ ai len field O)
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
