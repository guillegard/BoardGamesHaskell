import Control.Monad
import Data.List

-- Dado n numero de reinas, regresa las soluciones posibles para un tablero de n * n

reinas :: Int -> [[Int]]
reinas n = map fst $ foldM siguienteReina ([],[1..n]) [1..n]  where
  siguienteReina (y,d) _ = [(x:y, delete x d) | x <- d, verifica x]  where
    -- Verifica que no hay reinas
    verifica reina = and [reina /= c + n && reina /= c - n | (n,c) <- zip [1..] y]

-- Imprime el tablero resuelto
imprimirTableros y = do
     let n = length y
     mapM_ (\x -> putStrLn [if z == x then 'R' else '#' | z <- [1..n]]) y
     putStrLn ""

-- Imprime las soluciones para n reinas
main n = mapM_ imprimirTableros $ reinas n