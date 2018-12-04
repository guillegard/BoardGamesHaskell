import Control.Monad
import Data.List

-- Dado n numero de reinas, regresa las soluciones posibles para un tablero de n * n

reinas :: Int -> [[Int]]
reinas casilla = map fst $ foldM siguienteReina ([],[1..casilla]) [1..casilla]  where
  siguienteReina (caslla1,caslla2) _ = [(reina:caslla1, delete reina caslla2) | reina <- caslla2, verifica reina]  where
    -- Verifica que no hay reinas
    verifica reina = and [reina /= x + y && reina /= x - y | (y,x) <- zip [1..] caslla1]

-- Imprime el tablero resuelto
imprimirTableros tablero = do
     let longitud = length tablero
     mapM_ (\reina -> putStrLn [if casilla == reina then 'R' else '+' | casilla <- [1..longitud]]) tablero
     putStrLn ""

-- Imprime las soluciones para n reinas
main numeroDeReinas = mapM_ imprimirTableros $ reinas numeroDeReinas