import WebFudgets
import Haste.Graphics.Canvas
import MineSweeper
import GuiUtils
{-
data Tile = Bomb | Numeric Int
              deriving (Show, Eq)
-}
main = runF minefield

shellF title fud = boxF (h2F (textF title) >+ fud)

minefield = shellF "Minesweeper" $
    showF =<= fieldF
  where
    field = makeNewGuiField 15 15
    viewField = dataToGui field
    fieldF = tableF (length (rows' field)) buttonsF
    buttonsF = listF viewField
