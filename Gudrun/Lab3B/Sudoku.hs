module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )


example1 :: Sudoku
example1 =
      Sudoku
        [ [j 1,n,j 7,j 2,j 3,j 8,j 5,n,j 9]
        , [j 2,j 5,n,j 1,j 6,j 9,j 3,j 4,j 7]
        , [j 3,j 6,j 9,j 4,j 5,j 7,j 1,j 2,j 8]
        , [n,n,n,n,j 8,n,j 6,j 9,j 5]
        , [j 5,j 8,j 2,j 6,j 9,j 1,n,n,j 3]
        , [j 6,n,j 3,j 5,j 7,j 4,j 2,j 8,j 1]
        , [j 7,j 1,j 4,j 8,j 2,j 3,j 9,j 5,j 6]
        , [j 8,j 2,j 5,n,n,j 6,j 7,j 3,j 4]
        , [j 9,j 3,j 6,j 7,j 4,j 5,j 8,j 1,n]
        ]
    where
      n = Nothing
      j = Just

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
rNothing = return Nothing

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

-------------------------------------------------------------------------

type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = [(x,y) | x <- [0..8], y <- [0..8], isNothing(getPos (x,y) sud)]

getPos :: Pos -> Sudoku -> Maybe Int
getPos (x,y) sud = ((rows sud)!!y)!!x

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
      where list' = list !!= (i,element)

-- Checks if the sizes of old and new list are the same
prop_size_updateList:: [a] ->(NonNegative Int, a) -> Bool
prop_size_updateList list (NonNegative i,newE) = length list == length list'
          where list' = list !!= (i,newE)


-------------------------------------------------------------------------
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (posA, posB) newValue =
      Sudoku (rows sud !!= (posB, newRow))
            where oldRow = rows sud!!posB
                  newRow = oldRow !!= (posA, newValue)

--TODO: newValue has to be Valid
prop_update :: Sudoku -> ValidIndex -> ValidValue -> Bool
prop_update sud (ValidIndex (posA,posB)) (ValidValue newValue) =
                    updatedSud!!posB!!posA == newValue
    where updatedSud = rows (update sud (posA,posB) newValue)

data ValidIndex = ValidIndex Pos
    deriving ( Show, Eq )

instance Arbitrary ValidIndex where
    arbitrary =
      do  a <- choose(0,8)
          b <- choose(0,8)
          return (ValidIndex (a,b))

data ValidValue = ValidValue (Maybe Int)
  deriving (Show, Eq)

instance Arbitrary ValidValue where
     arbitrary =
       do  a <- cell
           return (ValidValue a)

validValues = [1..9]

candidates :: Sudoku -> Pos -> [Int]
candidates sud (posA,posB) = remove validValues blocks
    where block1 = rows sud!!posB
          block2 = transpose (rows sud)!!posA
          block3 = square (getIndex posA, getIndex posB) (rows sud)
          blocks = catMaybes (nub(block1++block2++block3))

--TODO: A better way to do this?
getIndex ::  Int -> Int
getIndex x | x `elem` [0..2] = 0
getIndex x | x `elem` [3..5] = 1
getIndex x | x `elem` [6..8] = 2

--Helper function that removes elements from list1 if they are in list2
remove :: Eq a => [a] -> [a] -> [a]
remove list1 list2 = [x | x<-list1, x `notElem` list2]

prop_candidates :: Sudoku -> ValidIndex -> Bool
prop_candidates sud (ValidIndex pos) | pos `elem` blanks sud = True
prop_candidates sud (ValidIndex pos) = and [isSudoku sud'  | sud' <- updatedSudokus]
      where cand = candidates sud pos
            updatedSudokus = [update sud pos (Just x)| x <-cand]

-------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve sud | not (isOkay sud)  = Nothing
solve sud                     = solve' sud

solve' :: Sudoku -> Maybe Sudoku
solve' sud | isSolved sud = Just sud
solve' sud = if noCandidates
              then Nothing
              else solve'' sud pos (c:candidates')
                where pos:blanks' = blanks sud
                      c:candidates' = candidates sud pos
                      noCandidates = null (c:candidates')

solve'' :: Sudoku ->
solve'' sud pos [] = Nothing
solve'' sud pos (c:candidates') | solve' (update sud pos (Just c)) == Nothing = solve'' sud pos candidates'
solve'' sud pos (c:candidates') = solve' (update sud pos (Just c))

readAndSolve :: FilePath -> IO ()
readAndSolve path = do  s <- readSudoku path
                        let a = fromJust (solve s)
                        printSudoku a

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 = isSolved sud1 && isOkay sud1 && sameStartingValues
  where  sameStartingValues = and [getPos(x,y) sud1 == getPos (x,y) sud2 | x<-[0..8], y<-[0..8],isJust (getPos (x,y) sud2)]

prop_SolveSound :: Sudoku -> Property
