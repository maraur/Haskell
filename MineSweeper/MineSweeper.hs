module MineSweeper where

import Test.QuickCheck hiding (shuffle)
import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)
import Data.Ord
import System.Random
import Data.List
import Data.Maybe
import System.Random.Shuffle

--REMOVE!!!!
import Debug.Trace
debug = flip trace
  --REMOVE!!!!

data Tile = Bomb | Numeric Int
              deriving (Show, Eq)

data MineField = MineField {rows :: [[Tile]]}
              deriving ( Show, Eq )

type Pos = (Int,Int)
{-
testFunction x y b = do g <- newStdGen
                 return makeBombField b (makeEmptyField x y) g
-}
example :: MineField
example =
      MineField
        [ [b  ,n 1,n 0,n 2,b  ,b  ,n 1,n 0,n 0]
        , [n 1,n 1,n 0,n 2,b  ,n 3,n 1,n 0,n 0]
        , [n 1,n 1,n 0,n 1,n 1,n 1,n 0,n 0,n 0]
        , [b  ,n 1,n 0,n 1,n 2,n 2,n 1,n 0,n 0]
        , [n 1,n 1,n 0,n 1,b  ,b  ,n 1,n 0,n 0]
        , [n 0,n 0,n 0,n 1,n 2,n 2,n 2,n 1,n 1]
        , [n 0,n 0,n 0,n 0,n 0,n 0,n 1,b  ,n 2]
        , [n 1,n 1,n 1,n 0,n 0,n 0,n 1,n 2,b  ]
        , [n 1,b  ,n 1,n 0,n 0,n 0,n 0,n 1,n 1]
        ]
    where
      n = Numeric
      b = Bomb

makeEmptyField :: Int -> Int -> MineField
makeEmptyField x y = MineField {rows = replicate y (replicate x (Numeric 0))}

-------------------------------------------------------------------------------

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (pos,val) = take pos xs ++ [val] ++ drop (pos+1) xs

prop_addedElem :: Eq a => [a] -> (NonNegative Int,a) -> Bool
prop_addedElem xs (NonNegative pos,val) | pos >= length xs = True
prop_addedElem xs (NonNegative pos,val) = ((xs !!= (pos,val)) !! pos) == val

prop_correctSize :: [a] -> (NonNegative Int, a) -> Bool
prop_correctSize xs (NonNegative pos,val) | pos >= length xs = True
prop_correctSize xs (NonNegative pos,val) =
                             length xs == length (xs !!= (pos,val))

updateTile :: Pos -> Tile -> MineField -> MineField
updateTile (x,y) val field = MineField(x' ++ [xRow !!= (x,val)] ++ x'')
      where xs  = rows field
            x'   = take y xs
            xRow = concat (take 1 (drop y xs))
            x''  = drop (y+1) xs

makeBombField :: Int -> MineField -> StdGen -> MineField
makeBombField numOfBombs field g = makeBombField' bombs field
    where bombs = makeShuffledCoordinates (makeCoordinates field) g numOfBombs


makeBombField' :: [Pos] -> MineField -> MineField
makeBombField' [] field = field
makeBombField' (x:pos) field = makeBombField' pos (updateTile x Bomb field)
-------------------------------------------------------------------------------
makeShuffledCoordinates :: [Pos] -> StdGen -> Int -> [Pos]
makeShuffledCoordinates coords g n = take n shuffled
                  where len = length coords
                        shuffled = shuffle' coords len g


makeCoordinates :: MineField -> [Pos]
makeCoordinates field = [(x,y) | y <- [0..(y'-1)], x <- [0..(x'-1)]]
    where x' = length (rows field)
          y' = length (transpose (rows field))

calculateField :: MineField -> MineField
calculateField = undefined

--calculateTile :: MineField -> Pos -> MineField
calculateTile field pos = updateTile pos (Numeric value) field
    where value = length (square field pos)

square :: MineField -> Pos -> [Tile]
square field pos = [getPos x field| x <- positions, getPos x field ==Bomb]
    where positions = square' field pos

square' :: MineField -> Pos -> [Pos]
square' field  (x,y) =
    [(a,b) | (a,b) <- fieldCoords, isClose x a, isClose y b, (x,y) /= (a,b) ]
      where fieldCoords = makeCoordinates field

isClose :: Int -> Int -> Bool
isClose x y = x `elem` [(y-1)..(y+1)]

getPos :: Pos -> MineField -> Tile
getPos (posX,posY) field = (rows field)!!posY!!posX

--------------------------------------------------------------------------------
--Just to see the field properly for debugging
printMineField :: MineField -> IO ()
printMineField field = mapM_ (putStrLn . makeLine) (rows field)

makeLine :: [Tile] -> String
makeLine = map makeChar

makeChar :: Tile -> Char
makeChar Bomb  = 'B'
makeChar (Numeric n) = intToDigit n
