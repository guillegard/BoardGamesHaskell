
import Data.List 
import System.IO

----------------------------------------------------------------------------------------
--VARIABLE A CAMBIAR PARA SELECCIONAR NIVEL DE DIFICULTAD

-- Definimos una variables para la busqueda. La definimos en 5 por defecto.
-- Esto define la dificultad de la ia, a mayor numero mejor sera pero mas lento.

busquedaMaxima :: Int
busquedaMaxima = 5

-----------------------------------------------------------------------------------------
-- LOGICA DEL JUGO

--Juego de dos jugadores el primero es X y el segundo O.
 
-- Las posiciones posibles seran una lista de numeros enteros del 1 al 9
-- Estas posiciones crean un teblero

type Pos = Int
type PosList = [Pos]

-- Un tablero se representa por la combinacion de posiciones de las fichas X y O 
 
data T = Tablero PosList PosList 
               deriving Show

--Definicion del tablero inicial
 
primeroTablero :: T
primeroTablero = Tablero [] []

-- Para saber si es turno de X. 
-- X es el primer jugador asi que cuando exista el mimso numero de X y O sera turno de X.
 
turnoDeprimerJugador :: T -> Bool
turnoDeprimerJugador (Tablero elementX elementO) = 
    length elementX == length elementO

-- poner una ficha en cierta posicion. 
 
ponerFicha :: T -> Pos -> T
ponerFicha (Tablero elementX elementO) p | turnoDeprimerJugador (Tablero elementX elementO) = Tablero (p:elementX) elementO
                   | otherwise            = Tablero elementX (p:elementO)

-- Verifica si se han colocado todas las fichas, el tablero esta lleno
 
tableroLleno :: T -> Bool
tableroLleno (Tablero elementX elementO) = length elementX + length elementO == 9
 
-- Determina si el primero es subconjuto del segundo

sub :: PosList -> PosList -> Bool
sub conjunto1 conjunto2 = all (`elem` conjunto2) conjunto1
 
-- Revisar si existe alguna linea en el tablero
 
revisionLinea :: PosList -> Bool
revisionLinea next = 
    sub [1,2,3] next ||sub [4,5,6] next ||sub [7,8,9] next ||sub [1,4,7] next ||
    sub [2,5,8] next ||sub [3,6,9] next ||sub [1,5,9] next ||sub [3,5,7] next
    

-- Revisar si hay algun ganador en el tablero

tableroConGanador :: T -> Bool
tableroConGanador (Tablero elementX elementO) = revisionLinea elementX || revisionLinea elementO

------------------------------------------------------------------------------
--ARBOL PARA JUEGO

--Se define el tipo Arbol
data A a = N a [A a]

--Cadena que muestra el arbol
 
cadenaDelA (N x elementX) = 
    show x ++ '\n' : (unlines . map ("  "++) . concatMap (lines . show)) elementX
 
instance Show a => Show (A a) where
  show = cadenaDelA

-- Muestra las posiciones que se encuentran sin ocupar en el tablero
 
posSinFicha :: T -> PosList
posSinFicha (Tablero elementX elementO) = [1..9] \\ (elementX++elementO)
 
-- Creacion de tableros que resultarian de ponerFichar un ficha en cualquier posicion libre
 
posiblesTableros :: T -> [T]
posiblesTableros t 
    | tableroConGanador t = [] 
    | otherwise      = map (ponerFicha t) (posSinFicha t)

--Constructor de arbol a partir de un tablero
 
generacionDeA :: T -> A T
generacionDeA t = 
    N t (map generacionDeA (posiblesTableros t)) 
 
--Evaluacion de un tablero
 
type Evaluacion = Int

-- obtencion de evaluaciones del arbol
 
evaluaciones :: [A (Evaluacion,T)] -> [Evaluacion]
evaluaciones ev = [v | N (v,_) _ <- ev]
 
-- Maximizacion del arbol mediante los evaluaciones.
 
maxEvaluaciones :: A T -> A (Evaluacion,T)
maxEvaluaciones (N t []) | tableroConGanador t = N (-1,t) []
                     | otherwise      = N (0,t) []                                        
maxEvaluaciones (N t ts) = N (maximum (evaluaciones ev),t) ev
    where ev = map minEvaluaciones ts
 
--Minimizacion del arbol mediante evaluaciones
 
minEvaluaciones :: A T -> A (Evaluacion,T)
minEvaluaciones (N t []) | tableroConGanador t = N (1,t) []
                     | otherwise      = N (0,t) []
minEvaluaciones (N t ts) = N (minimum (evaluaciones ev),t) ev
    where ev = map maxEvaluaciones ts
 
--Poda del arbol
 
podarA :: Int -> A a -> A a
podarA i (N x as)   | i == 0    = N x []
                    | otherwise = N x (map (podarA (i-1)) as)
 
--Selecciona la hijo de la raiz que tenga el mimso valore que la raiz
 
encuentra :: A (Evaluacion,T) -> T
encuentra (N (e,_) ts) = 
    head [t | N (e',t) _ <- ts, e'==e]
 
-- Mejor movimiento posible 
 
encuentraMejorPosicion :: T -> T
encuentraMejorPosicion = 
    encuentra . maxEvaluaciones . podarA busquedaMaxima . generacionDeA
 
------------------------------------------------------------------------------
--Print del tablero

--Muestra las posiciones
--Si ya hay ficha muestra la ficha de lo contrario muestra la posicion
 
imprimePosiciones :: T -> Pos -> String
imprimePosiciones (Tablero elementX elementO) p 
    | p `elem` elementX = "X"
    | p `elem` elementO = "O"
    | otherwise   = show p
 
--impresion de una linea para crear #
 
imprimeLineaPosiciones :: T -> [Pos] -> String
imprimeLineaPosiciones t = 
    concat . intersperse "|" . map (imprimePosiciones t)

--Funcion principal del print
 
imprimeTablero :: T -> String
imprimeTablero t = 
    "\n"++
    imprimeLineaPosiciones t [1..3] ++ "\n-----\n" ++
    imprimeLineaPosiciones t [4..6] ++ "\n-----\n" ++
    imprimeLineaPosiciones t [7..9] ++ "\n"
 
------------------------------------------------------------------------------
--Ejecucion principal para el juego

-- Main
-- Se pregunta al usuario si quiere ser el primero en tirar

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "¿Desea ser el prmero en jugar? (s/n) "
  input <- getLine
  putStrLn (imprimeTablero primeroTablero)
  if head input `elem` "sS"
     then usuario primeroTablero
     else ia primeroTablero

--Logica del juego, resive un tablero, prgunta en donde ponerFichar la ficha
--Se muestra nuevo tablero y se revisa si se acaba el juego.

usuario :: T -> IO ()
usuario t = do 
  putStr "\nIngresa posicion en donde colocar ficha "
  input <- getLine 
  putStr "\nMovimiento de usuario:"                                  
  let t' = ponerFicha t (read input :: Pos)          
  putStrLn (imprimeTablero t')                  
  if tableroConGanador t'                            
     then putStrLn "Ganaste! :D"                
     else if tableroLleno t'                        
             then putStrLn "Empate :|"            
             else ia t'                

--Resive un tablero y a partir de ahi busca el mejor movimiento.
--Se revisa si el juego termina

ia :: T -> IO ()
ia t = do
  putStrLn "\nComputadora buscando mejor movieminto.."            
  let t' = encuentraMejorPosicion t   
  putStrLn "\nMovimiento de ia:"        
  putStrLn (imprimeTablero t')       
  if tableroConGanador t'                 
     then putStrLn "Pierdes :("      
     else if tableroLleno t'             
             then putStrLn "Empate :|" 
             else usuario t'          
