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
              deriving (Show, Eq)

-- The Bool states whether the Tile is revealed or not
type GuiTile = (Bool, Tile)

data Shown = Flag | Showing Bool
               deriving (Show, Eq)

data GuiMineField = GuiMineField {rows' :: [[GuiTile]]}
              deriving (Show, Eq)

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

--Shouldn't be used!
makeNewGuiField :: Int -> Int -> GuiMineField
makeNewGuiField x y = GuiMineField {rows' = replicate y (replicate x (False, Numeric 0))}

makeGuiField :: MineField -> GuiMineField
makeGuiField field = GuiMineField(map makeGuiLine (rows field))

makeGuiLine :: [Tile] -> [GuiTile]
makeGuiLine = map makeGuiTile

makeGuiTile :: Tile -> GuiTile
makeGuiTile tile = (False, tile)
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
-- Functions for making the field with bombs
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
-- Functions for checking if game is over or won
isGameOver :: GuiMineField -> Bool
isGameOver field = or (map ((True,Bomb) `elem`) fieldRows)
      where fieldRows = rows' field

--TODO write this one
--This function should check if all non-bomb tiles are revealed
isGameWon :: GuiMineField -> Bool
isGameWon = undefined
--------------------------------------------------------------------------------
-- Functions for revealing and flaging tiles
--revealTileIO :: GuiMineField -> IO Pos -> GuiMineField
--revealTileIO

-- Recursively reveals tiles using the Flood Fill algorithm
revealTile :: GuiMineField -> Pos -> GuiMineField
revealTile guiLayer (x,y)
                | not (inBounds (x,y) guiLayer) || shown = guiLayer
                | otherwise = if tile /= (Numeric 0)
                                  then newGuiLayer
                                  else gridNorth
     where (shown,tile) = getGuiTile guiLayer (x,y)
           newGuiLayer  = showGuiTile guiLayer (x,y)
           gridEast     = revealTile newGuiLayer(x+1,y)
           gridWest     = revealTile gridEast (x-1,y)
           gridSouth    = revealTile gridWest (x,y+1)
           gridNorth    = revealTile gridSouth (x,y-1)
{-
flagTile :: GuiMineField -> Pos -> GuiMineField
flagTile guiLayer (x,y) | not (inBounds (x,y) guiLayer) || shown = guiLayer
                        | otherwise = updateGuiTile pos (True, val) field
-}
showGuiTile :: GuiMineField -> Pos -> GuiMineField
showGuiTile field pos | shown     = field
                      | otherwise = updateGuiTile pos (True, val) field
   where (shown,val) = getGuiTile field pos

updateGuiTile :: Pos -> GuiTile -> GuiMineField -> GuiMineField
updateGuiTile (x,y) val field = GuiMineField(x' ++ [xRow !!= (x,val)] ++ x'')
       where xs  = rows' field
             x'   = take y xs
             xRow = concat (take 1 (drop y xs))
             x''  = drop (y+1) xs

getGuiTile :: GuiMineField -> Pos -> GuiTile
getGuiTile field (posX,posY) = rows' field !! posY !! posX

-- checks if a position is inside the MineField
inBounds :: Pos -> GuiMineField -> Bool
inBounds (x,y) field = x >= 0 && y >= 0 && x <= xLen && y <= yLen
     where fieldRows = rows' field
           yLen = length (fieldRows) -1
           xLen = length (head fieldRows) -1
---------------------------------------------------------------------------------
--Printing stuff for GuiMineField
printGuiField :: GuiMineField -> IO ()
printGuiField field = mapM_ (putStrLn . makeLine') (rows' field)

makeLine' :: [GuiTile] -> String
makeLine' = map makeChar'

makeChar' :: GuiTile -> Char
makeChar' (False, _)  = '-'
makeChar' (_, Bomb)  = '*'
makeChar' (_, Numeric n) = intToDigit n
-----------------------------------------
-- DEBUG STUFF
testFunction = do
              g <- newStdGen
              let bombField = makeField 10 10 10 g
              let guiField = makeGuiField bombField
              printMineField bombField
              putStrLn "---------------------------"
              printGuiField guiField
              --x <- getChar
              --putStrLn [x]
            --  y <- getChar
              --putStrLn [y]
              let newField = revealTile guiField (0, 0)
              putStrLn "---------------------------"
              printGuiField newField
              return 0

--TODO REMOVE!!!! ONlY FOR DEBUG!
stringifyField :: GuiMineField -> String
stringifyField field = concatMap makeLineWithBreak (rows' field)

makeLineWithBreak :: [GuiTile] -> String
makeLineWithBreak list = makeLine' list ++ "\n"
