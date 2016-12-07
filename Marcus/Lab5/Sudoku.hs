module Sudoku where

import Test.QuickCheck
import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)
import Data.Ord
import System.Random
import Data.List
import Data.Maybe
--REMOVE!!!!
import Debug.Trace
debug = flip trace
--REMOVE!!!!
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
              isValidRowLength = all (\x -> length x == 9) rowsSud
              isValidNumber    = and [ and [x `elem` validNumbers | x <- x']
                                                | x' <- rowsSud]
              validNumbers     = Nothing : [Just n | n <- [1..9]]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and [isJust x | xs <- rows sud, x <- xs]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (putStrLn . makeLine) (rows sud)
--Remove
makeStringSudoku :: Sudoku -> String
makeStringSudoku sud = concatMap makeLineWithBreak (rows sud)

makeLineWithBreak :: [Maybe Int] -> String
makeLineWithBreak list = makeLine list ++ "\n"
--Remove
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

makeSudokuLine :: String -> [Maybe Int]
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
makeBlocks sud = [square (x,y) sud | y <- [0..2], x <- [0..2]]

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
blanks sud = [(x,y) | y <- [0..8], x <- [0..8], isNothing(getPos (x,y) sud)]

getPos :: Pos -> Sudoku -> Maybe Int
getPos (x,y) sud = (rows sud !! y) !! x

prop_areBlanks :: Sudoku -> Bool
prop_areBlanks sud = and [isNothing (getPos x sud) | x <- blanks sud]

-- Part E2

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (pos,val) = take pos xs ++ [val] ++ drop (pos+1) xs

prop_addedElem :: Eq a => [a] -> (NonNegative Int,a) -> Bool
prop_addedElem xs (NonNegative pos,val) | pos >= length xs = True
-- Better way to do this? ^^^^^^^^^^
prop_addedElem xs (NonNegative pos,val) = ((xs !!= (pos,val)) !! pos) == val

prop_correctSize :: [a] -> (NonNegative Int, a) -> Bool
prop_correctSize xs (NonNegative pos,val) | pos >= length xs = True
-- Better way to do this? ^^^^^^^^^^
prop_correctSize xs (NonNegative pos,val) =
                             length xs == length (xs !!= (pos,val))

prop_correctList :: Eq a => [a] -> (NonNegative Int, a) -> Bool
prop_correctList xs (NonNegative pos,val) | pos >= length xs = True
prop_correctList xs (NonNegative pos,val) = takePart && dropPart
        where xs'      = xs !!= (pos,val)
              takePart = (take pos xs') == (take pos xs)
              dropPart = (drop (pos+1) xs') == (drop (pos+1) xs)

-- Part E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (posA, posB) newValue = Sudoku (rows sud !!= (posB, newRow))
            where oldRow = rows sud!!posB
                  newRow = oldRow !!= (posA, newValue)

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
      where sudBlocks  = blocks sud
            rows       = sudBlocks !! y
            columns    = sudBlocks !! (9+x)
            candSquare = sudBlocks !! (18 + squareReg - 1)
            existing   = catMaybes (nub (rows ++ columns ++ candSquare))
            squareReg  = ((squareRegion x) + 1) + ((squareRegion y) * 3)

squareRegion :: Int -> Int
squareRegion x | x `elem` [0..2] = 0
               | x `elem` [3..5] = 1
               | x `elem` [6..8] = 2

prop_candidates :: Sudoku -> ValidPos -> Bool
prop_candidates sud (ValidPos pos) | pos `elem` blanks sud = True
prop_candidates sud (ValidPos pos) = and [isSudoku sud'  | sud' <- updatedSudokus]
      where cand           = candidates sud pos
            updatedSudokus = [update sud pos (Just x)| x <-cand]
---------------------------------------------------------------------------
-- Part F1
solve :: Sudoku -> Maybe Sudoku
solve sud | isOkay sud = solve' sud []
          | otherwise  = Nothing

solve' :: Sudoku -> [Maybe Int] -> Maybe Sudoku
solve' sud _            | isSolved sud         = Just sud
solve' sud []    = solve' sud [Just n | n <- candidates sud (head (blanks sud))]
solve' sud (cand:cands) | isSolved newSud       = Just newSud
                        | null cCands || isNothing result
                                                = if null cands
                                                  then Nothing
                                                  else solve' sud cands
                        | otherwise             = result
           where blanks'   = blanks sud
                 newSud    = update sud (head blanks') cand
                 cCands    = [Just n | n <- candidates newSud (blanks' !! 1)]
                 result    = solve' newSud cCands `debug` makeStringSudoku newSud
---------------------------------------------------------------------------
-- Part F2
readAndSolve :: FilePath -> IO ()
readAndSolve file = do
           s <- readSudoku file
           let result = solve s
           maybe (putStrLn "No solution") printSudoku result

--------------------------------------------------------------------------
-- Part F3
coordinates = [(x,y) | x <- [0..8], y <- [0..8]]
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solved _ | not(isOkay solved) || not(isSolved solved) = False
isSolutionOf solved unSolved = and [getPos p solved == getPos p unSolved | p <- nonBlanks]
            where nonBlanks = [n | n <- coordinates, n `notElem` blanks unSolved]


--------------------------------------------------------------------------
-- Part F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isOkay sud ==> isSolutionOf (fromJust (solve sud)) sud

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop
