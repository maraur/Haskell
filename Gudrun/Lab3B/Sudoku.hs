module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

example :: Sudoku
example =
     Sudoku
       [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
       , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
       , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
       , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
       , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
       , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
       , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
       , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
       , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
       ]
   where
     n = Nothing
     j = Just

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}

-------------------------------------------------------------------------

isSudoku :: Sudoku -> Bool
isSudoku sud = isValidRows x && isValidColumns x && isValidNumbers x
  where x = rows sud

isValidRows :: [[Maybe Int]] -> Bool
isValidRows x = length x == 9

isValidColumns :: [[Maybe Int]] -> Bool
isValidColumns xs = and [length x == 9 | x <- xs]

--Checks if all the numbers lie between 1 and 9
isValidNumbers :: [[Maybe Int]] -> Bool
isValidNumbers xxs = and [and [x `elem` [1..9] | Just x <- xs ] | xs <- xxs]

-------------------------------------------------------------------------

isSolved :: Sudoku -> Bool
isSolved sud = and [and [isJust x | x <- xs ] | xs <- rows sud]

-------------------------------------------------------------------------

printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (putStrLn . maybeToChar) (rows sud)

maybeToChar:: [Maybe Int] -> String
maybeToChar = map (maybe '.' intToDigit)

readSudoku :: FilePath -> IO Sudoku
readSudoku path = do  s <- readFile path
                      let l = lines s
                      let sud = Sudoku (map stringToMaybe l)
                      if not (isSudoku sud)
                        then error "Not sudoku"
                        else return sud

stringToMaybe :: String -> [Maybe Int]
stringToMaybe = map charToMaybe

charToMaybe :: Char -> Maybe Int
charToMaybe '.' = Nothing
charToMaybe n | isDigit n = Just (digitToInt n)
charToMaybe _ = error "Not sudoku"

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9,rNothing), (1,rNumeric)]

rNumeric :: Gen (Maybe Int)
rNumeric = elements [Just n | n <- [1..9]]

rNothing :: Gen (Maybe Int)
rNothing = elements [Nothing]

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
isOkayBlock []            = True
isOkayBlock (Nothing:xs)  = True && isOkayBlock xs
isOkayBlock (x:xs)        = (x `notElem` xs) && isOkayBlock xs

blocks :: Sudoku -> [Block]
blocks sud = sudokuRows ++ transpose sudokuRows ++ getBlocks sudokuRows
       where sudokuRows = rows sud

getBlocks :: [[Maybe Int]] -> [Block]
getBlocks rows = [square (x,y) rows | x <- [0..2], y <- [0..2]]

square :: (Int, Int) -> [[Maybe Int]] -> Block
square (x,y) rows = concat
          [take 3 (drop (3*x) row) | row <- take 3 (drop (3*y) rows)]

prop_validBlocks :: Sudoku -> Bool
prop_validBlocks sud = length sudokuBlocks == 27 &&
                 and [length x == 9| x <- sudokuBlocks]
                 where sudokuBlocks = blocks sud

isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (rows sud)

-------------------------------------------------------------------------

type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = [snd x | x <- addPosition sud, isNothing (fst x)]

--Helper function that adds the position of each element as the second element of a pair
addPosition :: Sudoku -> [(Maybe Int, Pos)]
addPosition sud = zip (concat (rows sud))
                            [(a,b) | a<-[0..8], b<-[0..8]]

prop_blanks:: Sudoku -> Bool
prop_blanks sud = and [isNothing ((rows sud!!a)!!b) | (a,b)<-blanks sud ]

-------------------------------------------------------------------------
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _                  = []
list !!= (i, _ ) | i<0 || i >= length list = list
list !!= (i, newElement) = take i list ++ [newElement] ++ drop (i+1) list

-- Checks if new element has actually been updated in the list
prop_updateList:: Eq a => [a] ->(NonNegative Int, a) -> Bool
prop_updateList list (NonNegative i, _) | i >= length list = True
prop_updateList list (NonNegative i, element) = list'!!i == element
      where list' = (list !!= (i,element))

-- Checks if the sizes of old and new list are the same
prop_size_updateList:: [a] ->(NonNegative Int, a) -> Bool
prop_size_updateList list (NonNegative i,newE) = length list == length list'
          where list' = (list !!= (i,newE))


-------------------------------------------------------------------------
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (posA, posB) newValue =
      Sudoku (rows sud !!= (posA, newRow))
            where oldRow = rows sud!!posA
                  newRow = oldRow !!= (posB, newValue)

prop_update :: Sudoku -> ValidIndex -> Maybe Int -> Bool
prop_update sud (ValidIndex (posA,posB)) newValue =
                    updatedSud!!posA!!posB == newValue
    where updatedSud = rows (update sud (posA,posB) newValue)

data ValidIndex = ValidIndex Pos
    deriving ( Show, Eq )

-- an instance for generating Arbitrary valid indices
instance Arbitrary ValidIndex where
    arbitrary =
      do  a <- choose(0,8)
          b <- choose(0,8)
          return (ValidIndex (a,b))

--candidates :: Sudoku -> Pos -> [Int]
--candidates sud (posA,posB) =
