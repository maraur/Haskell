module GuiUtils where

import MineSweeper
import WebFudgets
import Haste.Graphics.Canvas

data ViewTile = B | H | F | Printing Int
          deriving (Show, Eq)

--dataToGui :: GuiMineField -> [ViewTile]
dataToGui field = concat (map lineToGui (rows' field))

--lineToGui :: [GuiTile] -> [ViewTile]
lineToGui = map tileToGui

--tileToGui :: GuiTile -> (ViewTile, F t)
tileToGui (Nonvisible, _) = (H, buttonF (tileLabel H))
tileToGui (Flag, _) = (F, buttonF (tileLabel F))
tileToGui (_, Bomb) = (B, buttonF (tileLabel B))
tileToGui (_, Numeric i) = (Printing i, buttonF (show i))

tileLabel H = " "
tileLabel F = "F"
tileLabel B = "B"
