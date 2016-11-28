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
isSudoku sud = isValidRows && isValidColumns && isValidNumbers
  where x = rows sud
        isValidRows     = length x == 9
        isValidColumns  = and [length x' == 9 | x' <- x]
        isValidNumbers  = and [and [x'' `elem` [1..9] | Just x'' <- x' ] | x' <- x]

-------------------------------------------------------------------------

isSolved :: Sudoku -> Bool
isSolved sud = and [isJust x | xs <- rows sud, x <- xs ]

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
rNumeric = do n <- choose(1,9)
              return (Just n)

rNothing :: Gen (Maybe Int)
rNothing = return (Nothing)

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
isOkayBlock block = length list == length (nub list)
    where list = catMaybes block

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
