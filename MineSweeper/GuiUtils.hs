module GuiUtils where

import MineSweeper
import WebFudgets
import Haste.Graphics.Canvas

data ViewTile = B | H | F | Printing Int
          deriving (Show, Eq)

--dataToGui :: GuiMineField -> [ViewTile]
dataToGui field = concat [lineToGui i (fieldRows !! i) | i <- [0..len]]
       where fieldRows = rows' field
             len       = length (fieldRows) - 1


--lineToGui :: [GuiTile] -> [ViewTile]
lineToGui line xs = map tileToGui zipList
       where zipList = zip xs posList
             posList = makePosList line xs

makePosList :: Int -> [a] -> [Pos]
makePosList line xs = [(x,line) | x <- [0..len]]
       where len = (length xs) - 1

--tileToGui :: GuiTile -> (ViewTile, F t)
tileToGui ((Nonvisible, _), pos) = (pos, buttonF (tileLabel H))
tileToGui ((Flag, _), pos) = (pos, buttonF (tileLabel F))
tileToGui ((_, Bomb), pos) = (pos, buttonF (tileLabel B))
tileToGui ((_, Numeric i), pos) = (pos, buttonF (show i))

tileLabel H = " "
tileLabel F = "F"
tileLabel B = "B"
