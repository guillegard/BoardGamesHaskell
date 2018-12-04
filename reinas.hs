import Control.Monad
import Data.List

-- Dado n numero de reinas, regresa las soluciones posibles para un tablero de n * n

reinas :: Int -> [[Int]]
reinas n = map fst $ foldM siguienteReina ([],[1..n]) [1..n]  where

  siguienteReina (y,d) _ = [(x:y, delete x d) | x <- d, verifica x]  where

-- Verifica que no hay reinas
    verifica x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]

-- Imprime el tablero resuelto
     let n = length y
     mapM_ (\x -> putStrLn [if z == x then 'Q' else '-' | z <- [1..n]]) y
     putStrLn ""

-- Imprime las soluciones para n reinas
main n = mapM_ printSolution $ reinas n
