module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
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


example1 :: Sudoku
example1 =
          Sudoku
            [ [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            , [j 3,j 6,j 9,j 5,j 7,j 1,j 2,j 2,j 4]
            ]
        where
          n = Nothing
          j = Just

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}

-------------------------------------------------------------------------
-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
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
-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and [and [x /= Nothing | x <- xs ] | xs <- rows sud]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ putStrLn (map maybeToChar (rows sud))

maybeToChar:: [Maybe Int] -> String
maybeToChar row = map (maybe '.' intToDigit) row

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do  s <- readFile path
                      let l = lines s
                      return (Sudoku (map stringToMaybe l))

stringToMaybe :: [Char] -> [Maybe Int]
stringToMaybe s = map charToMaybe s

charToMaybe :: Char -> Maybe Int
charToMaybe '.' = Nothing
charToMaybe n | isDigit n = Just (digitToInt n)

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
--cell = elements [Just n|n<-[1..9]]
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
