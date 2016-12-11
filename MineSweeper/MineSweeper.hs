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
--makeBombField' [] field       = field
--makeBombField' (x:pos) field  = makeBombField' pos (updateTile x Bomb field)
makeBombField' pos field = foldl (\ field x -> updateTile x Bomb field) field pos
-- ^ according to hlint

-- Single function for generating the field
makeField :: Int -> Int -> Int -> StdGen -> MineField
makeField x y bombs gen = calculateField bombField
    where emptyField = makeEmptyField x y
          bombField  = makeBombField bombs emptyField gen
-------------------------------------------------------------------------------
makeShuffledCoordinates :: [Pos] -> StdGen -> Int -> [Pos]
makeShuffledCoordinates coords g n = take n shuffled
                  where len      = length coords
                        shuffled = shuffle' coords len g


makeCoordinates :: MineField -> [Pos]
makeCoordinates field = [(x,y) | y <- [0..(y'-1)], x <- [0..(x'-1)]]
    where x' = length (rows field)
          y' = length (transpose (rows field))

calculateField :: MineField -> MineField
calculateField field = calculateField' coords field
    where coords = makeCoordinates field

calculateField' :: [Pos] -> MineField -> MineField
calculateField' [] field = field
calculateField' (x:pos) field = if getPos x field == Bomb
                                  then
                                    calculateField' pos field
                                  else
                                    calculateField' pos (updateTile x value field)
        where value = calculateTile field x

--TODO
prop_calculateField :: MineField -> Bool
prop_calculateField = undefined

calculateTile :: MineField -> Pos -> Tile
calculateTile field pos = Numeric value
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
getPos (posX,posY) field = rows field!!posY!!posX

--------------------------------------------------------------------------------
--Just to see the field properly for debugging
printMineField :: MineField -> IO ()
printMineField field = mapM_ (putStrLn . makeLine) (rows field)

makeLine :: [Tile] -> String
makeLine = map makeChar

makeChar :: Tile -> Char
makeChar Bomb  = 'B'
makeChar (Numeric n) = intToDigit n
--------------------------------------------------------------------------------
isGameOver :: MineField -> Bool
isGameOver field = or (map (Bomb `elem`) fieldRows)
      where fieldRows = rows field

-- Recursively reveals tiles using the Flood Fill algorithm
revealTile :: MineField -> MineField -> Pos -> MineField
revealTile guiLayer bombLayer (x,y)
                | not (inBounds (x,y) bombLayer) = guiLayer `debug` "bug"
                | otherwise = guiLayer `debug` "yaay"
                {-
                | otherwise      = if tile /= (Numeric 0)
                                       then newGuiLayer `debug` (stringifyField newGuiLayer)
                                       else gridNorth `debug` (stringifyField gridNorth) -}
     where tile        = getPos (x,y) bombLayer
           newGuiLayer = updateTile (x,y) tile guiLayer
           gridEast    = revealTile newGuiLayer bombLayer (x+1,y)
           gridWest    = revealTile gridEast bombLayer (x-1,y)
           gridSouth   = revealTile gridWest bombLayer (x,y+1)
           gridNorth   = revealTile gridSouth bombLayer (x,y-1)

-- checks if a position is inside the MineField
inBounds :: Pos -> MineField -> Bool
inBounds (x,y) field = x >= 0 && y >= 0 && x <= xLen && y <= yLen
     where fieldRows = rows field
           yLen = length (fieldRows) -1
           xLen = length (head fieldRows) -1
{-
 g <- newStdGen
let empty = makeEmptyField 10 10
let bombField = makeField 10 10 10 g
 printMineField bombField
let newField = revealTile empty bombField (0,2)
 printMineField newField
 -}

--TODO REMOVE!!!! ONlY FOR DEBUG!
stringifyField :: MineField -> String
stringifyField field = concatMap makeLineWithBreak (rows field)

makeLineWithBreak :: [Tile] -> String
makeLineWithBreak list = makeLine list ++ "\n"
