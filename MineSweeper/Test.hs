import Haste
import Haste.DOM
import Haste.Events
import MineSweeper
import System.Random
import WebFudgets
import Data.Maybe
import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)

import Pages

main = do
    g <- newStdGen
    input  <- mkInput 30 "Position of button"
    let field = calculateField (makeBombField 3 (makeEmptyField 3 3) g)
    let coords = makeCoordinates field
    --let buttons = mkButtons field
    button1 <- mkButton " " (0,0)
    button2 <- mkButton " " (1,0)
    button3 <- mkButton " " (2,0)
    button4 <- mkButton " " (0,1)
    button5 <- mkButton " " (1,1)
    button6 <- mkButton " " (2,1)
    button7 <- mkButton " " (0,2)
    button8 <- mkButton " " (1,2)
    button9 <- mkButton " " (2,2)

    inputRow  <- newElem "div"
    topRow    <- newElem "div"
    middleRow <- newElem "div"
    bottomRow <- newElem "div"

    row inputRow  [input]
    row topRow    [button1, button2, button3]
    row middleRow [button4, button5, button6]
    row bottomRow [button7, button8, button9]

    column documentBody [inputRow, topRow, middleRow, bottomRow]

    onEvent button1 Click $ \_ -> do update button1 input field
    onEvent button2 Click $ \_ -> do update button2 input field
    onEvent button3 Click $ \_ -> do update button3 input field
    onEvent button4 Click $ \_ -> do update button4 input field
    onEvent button5 Click $ \_ -> do update button5 input field
    onEvent button6 Click $ \_ -> do update button6 input field
    onEvent button7 Click $ \_ -> do update button7 input field
    onEvent button8 Click $ \_ -> do update button8 input field
    onEvent button9 Click $ \_ -> do update button9 input field

update btn input field = do
                      strPos <- getProp btn "btnID"
                      let Numeric el = getElement (toPos strPos) field
                      set btn [ prop "innerHTML" =: show el ]
                      return ()

--mkButtons :: MineField -> [IO Elem]
--mkButtons field = [mkButton (getElement pos field) pos | pos <- coords ]
--    where coords = makeCoordinates field
toPos xs = (x,y)
        where x = digitToInt (xs!!1)
              y = digitToInt (xs!!3)

mkButton :: String -> Pos -> IO Elem
mkButton label pos = do
          button <- newElem "button"
          set button [ prop "innerHTML" =: label, prop "btnID" =: show pos]
          return button
