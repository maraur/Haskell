module Sudoku where

import Test.QuickCheck
import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)
import Data.Ord
import System.Random
import Data.List
import Data.Maybe
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
isSudoku sud = isValidLength && isValidRowLength
        where rowsSud          = rows sud
              isValidLength    = length rowsSud == 9
              isValidRowLength = all (\x -> (length x == 9)) (rowsSud)
              isValidNumber    = and [ and [x `elem` validNumbers | x <- x']
                                                | x' <- rowsSud]
              validNumbers     = [Nothing] ++ [Just n | n <- [1..9]]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and [isJust x | xs <- rows sud, x <- xs]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (putStrLn . makeLine) (rows sud)

makeLine :: [Maybe Int] -> String
makeLine = map makeChar

makeChar :: Maybe Int -> Char
makeChar Nothing  = '.'
makeChar (Just n) = intToDigit n

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do s <- readFile file
                     let l = lines s
                     let sud = Sudoku(map makeSudokuLine l)
                     if not(isSudoku sud)
                       then error "Program error: Not a Sudoku!"
                       else return sud

makeSudokuLine :: [Char] -> [Maybe Int]
makeSudokuLine = map makeSudokuChar

makeSudokuChar :: Char -> Maybe Int
makeSudokuChar '.'           = Nothing
makeSudokuChar n | isDigit n = Just (digitToInt n)
makeSudokuChar _             = error "Program error: Not a Sudoku!"
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, nothingCell),(1,numberCell)]

nothingCell :: Gen (Maybe Int)
nothingCell = return Nothing

numberCell :: Gen (Maybe Int)
numberCell = do n <- choose(1,9)
                return (Just n)

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
isOkayBlock block = length xs == length (nub xs)
                where xs = filter (/= Nothing) block


blocks :: Sudoku -> [Block]
blocks sud = sudokuRows ++ transpose sudokuRows ++ makeBlocks sud
       where sudokuRows = rows sud

makeBlocks :: Sudoku -> [Block]
makeBlocks sud = [square (x,y) sud | x <- [0..2], y <- [0..2]]

square :: (Int, Int) -> Sudoku -> Block
square (x,y) sud = concat
          [take 3 (drop (3*x) row) | row <- take 3 (drop (3*y) (rows sud))]

prop_validBlocks :: Sudoku -> Bool
prop_validBlocks sud = length sudokuBlocks == 27 &&
                 and [length x == 9| x <- sudokuBlocks]
                 where sudokuBlocks = blocks sud

isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock block | block <- blocks sud]
---------------------------------------------------------------------------
-- Part E
---------------------------------------------------------------------------
-- Part E1
type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = [(x,y) | x <- [0..8], y <- [0..8], isNothing(getPos (x,y) sud)]

getPos :: Pos -> Sudoku -> Maybe Int
getPos (x,y) sud = ((rows sud)!!y)!!x

prop_areBlanks :: Sudoku -> Bool
prop_areBlanks sud = and [getPos x sud == Nothing | x <- blanks sud]

-- Part E2

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (pos,val) = take pos xs ++ [val] ++ drop (pos+1) xs

prop_addedElem :: Eq a => [a] -> (NonNegative Int,a) -> Bool
prop_addedElem xs (NonNegative pos,val) | pos >= length xs = True
-- Better way to do this? ^^^^^^^^^^
prop_addedElem xs (NonNegative pos,val) = (((xs !!= (pos,val)) !! pos) == val)

prop_correctSize :: [a] -> (NonNegative Int, a) -> Bool
prop_correctSize xs (NonNegative pos,val) | pos >= length xs = True
-- Better way to do this? ^^^^^^^^^^
prop_correctSize xs (NonNegative pos,val) =
                             (length xs) == length (xs !!= (pos,val))

-- Part E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (x,y) val = Sudoku(x' ++ [(xRow !!= (x,val))] ++ x'')
              where xs  = rows sud
                    x'   = take y xs
                    xRow = concat (take 1 (drop y xs))
                    x''  = drop (y+1) xs

prop_update :: Sudoku -> ValidPos -> ValidValue -> Bool
prop_update sud (ValidPos pos) (ValidValue val)
                               = getPos pos (update sud pos val) == val

data ValidPos = ValidPos Pos
    deriving ( Show, Eq )

instance Arbitrary ValidPos where
    arbitrary =
      do  a <- choose(0,8)
          b <- choose(0,8)
          return (ValidPos (a,b))

data ValidValue = ValidValue (Maybe Int)
    deriving ( Show, Eq )
-- Really needed, might be able to just use Cell somehow?
instance Arbitrary ValidValue where
     arbitrary =
       do  a <- cell
           return (ValidValue a)

-- Part E4
candidates :: Sudoku -> Pos -> [Int]
candidates sud (x,y) = [x | x <- [1..9], x `notElem` existing]
      where rowX       = (rows sud) !! y
            rowY       = (transpose (rows sud)) !! x
            candSquare = square (squareRegion x, squareRegion y) sud
            existing   = catMaybes (nub (rowX ++ rowY ++ candSquare))

squareRegion :: Int -> Int
squareRegion x | x `elem` [0..2] = 0
               | x `elem` [3..5] = 1
               | x `elem` [6..8] = 2
---------------------------------------------------------------------------
