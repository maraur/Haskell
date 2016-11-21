module Sudoku where

import Test.QuickCheck
import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)
import Data.Ord
import System.Random
import Data.List
-------------------------------------------------------------------------
example =
  Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku[[Nothing | x <- [1..9]] | y <- [1..9]]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = (length (rows sud) == 9) &&
                  (all (\x -> (length x == 9) && (isValidRow x)) (rows sud))

isValidRow :: [Maybe Int] -> Bool
isValidRow = all (\x -> isValidNumber x)

isValidNumber :: Maybe Int -> Bool
isValidNumber Nothing = True
isValidNumber (Just n) = (n < 10) && (n > 0)

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = all (\x -> isRowSolved x) (rows sud)

isRowSolved :: [Maybe Int] -> Bool
isRowSolved = all (\x -> x /= Nothing)

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (putStrLn . makeLine) (rows sud)

makeLine :: [Maybe Int] -> String
makeLine = map makeChar

makeChar :: Maybe Int -> Char
makeChar Nothing = '.'
makeChar (Just n) = intToDigit n

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do s <- readFile file
                     let l = lines s
                     let sud = Sudoku(map makeSudokuLine l)
                     return sud

makeSudokuLine :: [Char] -> [Maybe Int]
makeSudokuLine = map makeSudokuChar

makeSudokuChar :: Char -> Maybe Int
makeSudokuChar '.' = Nothing
makeSudokuChar n | isDigit n = Just (digitToInt n)
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, nothingCell),(1,numberCell)]

nothingCell :: Gen (Maybe Int)
nothingCell = elements [Nothing]

numberCell :: Gen (Maybe Int)
numberCell = elements [Just n|n<-[1..9]]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
-------------------------------------------------------------------------
type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock []          = True
isOkayBlock (Nothing:xs) = True && isOkayBlock xs
isOkayBlock (x:xs)      = (x `notElem` xs) && isOkayBlock xs

blocks :: Sudoku -> [Block]
blocks sud = sudokuRows ++ (transpose sudokuRows) ++ makeBlocks sud
       where sudokuRows = rows sud

makeBlocks :: Sudoku -> [Block]
makeBlocks sud = [square (x,y) sud | x <- [0..2], y <- [0..2]]

square :: (Int, Int) -> Sudoku -> Block
square (x,y) sud = concat
          [take 3 (drop (3*x) row) | row <-(take 3 (drop (3*y) (rows sud)))]

prop_validBlocks :: Sudoku -> Bool
prop_validBlocks sud = length (sudokuBlocks) == 27 &&
                 and [length x == 9| x <- sudokuBlocks]
                 where sudokuBlocks = blocks sud

isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock block | block <- blocks sud]
