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

data Tile = Bomb | Numeric Integer
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


-- TODO this needs to be recursive
--makeBombField :: Int -> MineField -> StdGen -> MineField
--makeBombField b old g = (\x -> updateTile x bomb old) bombs
--          where bombs = makeShuffledCoordinates old g b

updateTile :: Pos -> Tile -> MineField -> MineField
updateTile (x,y) val field = MineField(x' ++ [xRow !!= (x,val)] ++ x'')
              where xs  = rows field
                    x'   = take y xs
                    xRow = concat (take 1 (drop y xs))
                    x''  = drop (y+1) xs

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (pos,val) = take pos xs ++ [val] ++ drop (pos+1) xs

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
getPos = undefined
