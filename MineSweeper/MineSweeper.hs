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
    where bombs = makeShuffledCoordinates (makeCoordinates x y) g numOfBombs
          x = length (rows field)
          y = length (transpose (rows field))

makeBombField' :: [Pos] -> MineField -> MineField
makeBombField' [] field = field
makeBombField' (x:pos) field = makeBombField' pos (updateTile x Bomb field)
-------------------------------------------------------------------------------
makeShuffledCoordinates :: [Pos] -> StdGen -> Int -> [Pos]
makeShuffledCoordinates coords g n = take n shuffled
                  where len = length coords
                        shuffled = shuffle' coords len g


makeCoordinates :: Int -> Int -> [Pos]
makeCoordinates x y = [(x,y) | y <- [0..(y-1)], x <- [0..(x-1)]]

calculateField :: MineField -> MineField
calculateField = undefined

calculateTile :: MineField -> Pos -> MineField
calculateTile = undefined

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
