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

type GuiTile = (Status, Tile)

data Status = Flag | Visible | Nonvisible
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
makeNewGuiField x y = GuiMineField {rows' = replicate y (replicate x (Nonvisible, Numeric 0))}

makeGuiField :: MineField -> GuiMineField
makeGuiField field = GuiMineField(map makeGuiLine (rows field))

makeGuiLine :: [Tile] -> [GuiTile]
makeGuiLine = map makeGuiTile

makeGuiTile :: Tile -> GuiTile
makeGuiTile tile = (Nonvisible, tile)

cell :: Gen Tile
cell = frequency [(9, numberCell),(1,bombCell)]

bombCell :: Gen Tile
bombCell = return Bomb

numberCell :: Gen Tile
numberCell = do n <- choose(0,8)
                return (Numeric n)

-- an instance for generating Arbitrary MineFields
instance Arbitrary MineField where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (MineField rows)

instance Arbitrary Tile where
    arbitrary =
      do  a <- cell
          return a

data ValidPos = ValidPos Pos
      deriving ( Show, Eq )

instance Arbitrary ValidPos where
      arbitrary =
            do  a <- choose(0,8)
                b <- choose(0,8)
                return (ValidPos (a,b))
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
updateTile (x,y) value field = MineField (rows field !!= (y,newRow))
        where oldRow = rows field!!y
              newRow = oldRow !!= (x, value)

prop_updateTile :: MineField -> ValidPos -> Tile -> Bool
prop_updateTile field (ValidPos (x,_)) _ | x >= length (rows field) = True
prop_updateTile field (ValidPos (_,y)) _ | y >= length (transpose (rows field)) = True
prop_updateTile field (ValidPos pos) val = getElement pos newField == val
                where newField = updateTile pos val field

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
calculateField' (x:pos) field = if getElement x field == Bomb
                                  then
                                    calculateField' pos field
                                  else
                                    calculateField' pos (updateTile x value field)
        where value = calculateTile field x

--TODO
--prop_calculateField :: MineField -> Bool
prop_calculateField field =
   [isCorrectValue field pos| pos <- makeCoordinates field, isNumeric (getElement pos field)]

isNumeric :: Tile -> Bool
isNumeric (Numeric tile) = True
isNumeric _ = False

isCorrectValue :: MineField -> Pos -> Bool
isCorrectValue field pos = getElement pos field == calculateTile field pos

calculateTile :: MineField -> Pos -> Tile
calculateTile field pos = Numeric value
    where value = length (square field pos)

square :: MineField -> Pos -> [Tile]
square field pos = [getElement x field| x <- positions, getElement x field == Bomb]
    where positions = square' field pos

square' :: MineField -> Pos -> [Pos]
square' field  (x,y) =
    [(a,b) | (a,b) <- fieldCoords, isClose x a, isClose y b, (x,y) /= (a,b) ]
      where fieldCoords = makeCoordinates field

isClose :: Int -> Int -> Bool
isClose x y = x `elem` [(y-1)..(y+1)]

getElement :: Pos -> MineField -> Tile
getElement (posX,posY) field = rows field!!posY!!posX

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
isBomb :: Tile -> Bool
isBomb tile = tile == Bomb

getNonBombsPos :: MineField -> [Pos]
getNonBombsPos field = [x |  x <- coords, not (isBomb (getElement x field))]
    where coords = makeCoordinates field

getBombsPos :: MineField -> [Pos]
getBombsPos field = [x |  x <- coords, isBomb (getElement x field)]
    where coords = makeCoordinates field

prop_getBombsPos :: MineField -> Bool
prop_getBombsPos field = and [isBomb (getElement x field) | x <- bombsPos]
    where bombsPos = getBombsPos field
--------------------------------------------------------------------------------
-- Functions for checking if game is over or won
isGameOver :: GuiMineField -> Bool
isGameOver field = or (map ((Visible,Bomb) `elem`) fieldRows)
      where fieldRows = rows' field

--TODO write this one
--This function should check if all non-bomb tiles are revealed
isGameWon :: GuiMineField -> Bool
isGameWon field = and [(snd x == Bomb) | x <- fieldFilt]
     where fieldCon  = concat (rows' field)
           fieldFilt = [ x | x <- fieldCon, (fst x) == Nonvisible]
--------------------------------------------------------------------------------
-- Functions for revealing and flaging tiles

-- Recursively reveals tiles using the Flood Fill algorithm
revealTile :: GuiMineField -> Pos -> GuiMineField
revealTile guiLayer (x,y)
                | not (inBounds (x,y) guiLayer) || shown = guiLayer
                | otherwise = if tile /= (Numeric 0)
                                  then newGuiLayer
                                  else gridNorth
     where (vis,tile) = getGuiTile guiLayer (x,y)
           shown = vis == Visible
           newGuiLayer   = showGuiTile guiLayer (x,y)
           gridEast      = revealTile newGuiLayer(x+1,y)
           gridWest      = revealTile gridEast (x-1,y)
           gridSouth     = revealTile gridWest (x,y+1)
           gridNorthEast = revealTile gridSouth (x+1,y-1)
           gridNorthWest = revealTile gridNorthEast (x-1,y-1)
           gridSouthEast = revealTile gridNorthWest (x+1,y+1)
           gridSouthWest = revealTile gridNorthEast (x-1,y+1)
           gridNorth     = revealTile gridSouthWest (x,y-1)

flagTile :: GuiMineField -> Pos -> GuiMineField
flagTile field pos | not (inBounds pos field) || shown = field
                   | otherwise = updateGuiTile field pos (Flag, val)
      where (vis,val) = getGuiTile field pos
            shown     = vis == Visible


showGuiTile :: GuiMineField -> Pos -> GuiMineField
showGuiTile field pos | shown     = field
                      | otherwise = updateGuiTile field pos (Visible, val)
       where (vis,val) = getGuiTile field pos
             shown     = vis == Visible

updateGuiTile :: GuiMineField -> Pos -> GuiTile -> GuiMineField
updateGuiTile field (x,y) val  = GuiMineField (rows' field !!= (y,newRow))
        where oldRow = rows' field!!y
              newRow = oldRow !!= (x, val)

getGuiTile :: GuiMineField -> Pos -> GuiTile
getGuiTile field (posX,posY) = rows' field !! posY !! posX

-- checks if a position is inside the MineField
--TODO do we still need this?
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
makeChar' (Nonvisible, _)  = '-'
makeChar' (Flag, _) = 'F'
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
              let won = isGameWon newField
              return won

--TODO REMOVE!!!! ONlY FOR DEBUG!
stringifyField :: GuiMineField -> String
stringifyField field = concatMap makeLineWithBreak (rows' field)

makeLineWithBreak :: [GuiTile] -> String
makeLineWithBreak list = makeLine' list ++ "\n"
