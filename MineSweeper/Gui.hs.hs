import WebFudgets
import Haste.Graphics.Canvas
--import MineSweeper

data Tile = Bomb | Numeric Int
              deriving (Show, Eq)

main = runF minefield

shellF title fud = boxF (h2F (textF title) >+ fud)

minefield = shellF "Minesweeper" $
      showF =<=  fieldF
  where
    fieldF = tableF 9 buttonsF
    buttonsF = listF [  b  ,n 1,n 0,n 2,b  ,b  ,n 1,n 0,n 0
                      , n 1,n 1,n 0,n 2,b  ,n 3,n 1,n 0,n 0
                      , n 1,n 1,n 0,n 1,n 1,n 1,n 0,n 0,n 0
                      , b  ,n 1,n 0,n 1,n 2,n 2,n 1,n 0,n 0
                      , n 1,n 1,n 0,n 1,b  ,b  ,n 1,n 0,n 0
                      , n 0,n 0,n 0,n 1,n 2,n 2,n 2,n 1,n 1
                      , n 0,n 0,n 0,n 0,n 0,n 0,n 1,b  ,n 2
                      , n 1,n 1,n 1,n 0,n 0,n 0,n 1,n 2,b
                      , n 1,b  ,n 1,n 0,n 0,n 0,n 0,n 1,n 1]

    n i = (Numeric i,buttonF (show i))
    b = (Bomb,buttonF (show 'B'))
