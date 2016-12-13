import WebFudgets
import Haste.Graphics.Canvas
import MineSweeper
import GuiUtils
import System.Random
import Haste.DOM
import Haste.Events
import Data.IORef

--main :: IO()
main = do g <- newStdGen
          runF (minefield g)


shellF title fud = boxF (h2F (textF title) >+ fud)

minefield g = shellF "Minesweeper" $
    showF =<= fieldF
  where
    field = calculateField (makeBombField 10 (makeEmptyField 9 9) g)
    guiField = makeGuiField field
    viewField = dataToGui guiField
    buttonsF = listF viewField
    fieldF = tableF (length (rows' guiField)) buttonsF
