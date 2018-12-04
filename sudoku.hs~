#!/usr/bin/env runhaskell

-- Note: For a slight improvement in performance, import the non-portable
-- Data.Array.Unboxed module instead of Data.Array, and change the Board
-- type below to use "UArray" instead of "Array".
import Data.Array

-- Solve the example puzzle specified below
-- TODO: read puzzle from input
main = do
    let solution = solve puzzleBoard
    printBoard solution

-- The marks on the board are represented by Ints in the range 0..9, where 0 represents "empty".
type Mark = Int

-- A square is identified by a (row, column) pair
type Location = (Int, Int)

-- A sudoku board is a 9x9 matrix of marks
type Board = Array Location Mark

-- The sudoku board to be solved
puzzleBoard :: Board
puzzleBoard = array ((0, 0), (8, 8)) $ puzzleAssocs examplePuzzle

-- Example puzzle from http://en.wikipedia.org/wiki/Sudoku
examplePuzzle :: [[Mark]]
examplePuzzle = [[5, 3, 0,  0, 7, 0,  0, 0, 0],
                 [6, 0, 0,  1, 9, 5,  0, 0, 0],
                 [0, 9, 8,  0, 0, 0,  0, 6, 0],

                 [8, 0, 0,  0, 6, 0,  0, 0, 3],
                 [4, 0, 0,  8, 0, 3,  0, 0, 1],
                 [7, 0, 0,  0, 2, 0,  0, 0, 6],

                 [0, 6, 0,  0, 0, 0,  2, 8, 0],
                 [0, 0, 0,  4, 1, 9,  0, 0, 5],
                 [0, 0, 0,  0, 8, 0,  0, 7, 0]]

-- Return first solution, or Nothing if no solutions found
solve :: Board -> Maybe Board
solve = headOrNothing . solutions

-- Return all solutions
solutions :: Board -> [Board]
solutions b = solutions' (emptyLocations b) b
  where
    -- Given list of empty locations on a board, pick an empty location,
    -- determine which marks can be put in that location, and then
    -- recursively find all solutions for that set of marks.
    solutions' :: [Location] -> Board -> [Board]
    solutions' []     b = [b]
    solutions' (x:xs) b = concatMap (solutions' xs) candidateBoards
      where
        candidateMarks  = [m | m <- [1..9], isPossibleMark m x b]
        candidateBoards = map (\m -> copyWithMark m x b) candidateMarks

-- Return list of locations where value is 0
emptyLocations :: Board -> [Location]
emptyLocations b = [(row, col) | row <- [0..8], col <- [0..8], b ! (row, col) == 0]

-- Determine whether the specified mark can be placed at specified position
isPossibleMark :: Mark -> Location -> Board -> Bool
isPossibleMark m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m $ b `marksInRow` row
    notInColumn = notElem m $ b `marksInColumn` col
    notInBox    = notElem m $ b `marksIn3x3Box` (row, col)

-- Return board with specified value in specified Location
copyWithMark :: Mark -> Location -> Board -> Board
copyWithMark mark (row, col) b = b // [((row, col), mark)]

-- Return the marks in the specified row
marksInRow :: Board -> Int -> [Mark]
b `marksInRow` row = [b ! loc | loc <- range((row, 0), (row, 8))]

-- Return the marks in the specified column
marksInColumn ::  Board -> Int -> [Mark]
b `marksInColumn` col = [b ! loc | loc <- range((0, col), (8, col))]

-- Return the marks in the 3x3 box that includes the specified Location
marksIn3x3Box :: Board -> Location -> [Mark]
b `marksIn3x3Box` (row, col) = [b ! loc | loc <- locations]
  where
    row' = (row `div` 3) * 3
    col' = (col `div` 3) * 3
    locations = range((row', col'), (row' + 2, col' + 2))

-- Convert a list of rows of marks (as in examplePuzzle above) to a list of array associations
puzzleAssocs :: [[Mark]] -> [(Location, Mark)]
puzzleAssocs = concatMap rowAssocs . zip [0..8]
  where
    rowAssocs :: (Int, [Mark]) -> [((Int, Int), Mark)]
    rowAssocs (row, marks) = colAssocs row $ zip [0..8] marks

    colAssocs :: Int -> [(Int, Mark)] -> [((Int, Int), Mark)]
    colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

printBoard :: Maybe Board -> IO ()
printBoard Nothing  = putStrLn "No solution"
printBoard (Just b) = mapM_ putStrLn [show $ b `marksInRow` row | row <- [0..8]]

printSolution y = do
     let n = length y
     mapM_ (\x -> putStrLn [if z == x then 'Q' else '-' | z <- [1..n]]) y
     putStrLn ""

