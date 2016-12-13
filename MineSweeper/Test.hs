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
    let field = calculateField (makeBombField 7 (makeEmptyField 5 5) g)
    --let coords = makeCoordinates field
    --let buttons = mkButtons field
    actionButton <- newElem "button"
    set actionButton [ prop "innerHTML" =: "Press"]


    button1 <- mkButton  " " (0,0)
    button2 <- mkButton  " " (1,0)
    button3 <- mkButton  " " (2,0)
    button4 <- mkButton  " " (3,0)
    button5 <- mkButton  " " (4,0)
    button6 <- mkButton  " " (0,1)
    button7 <- mkButton  " " (1,1)
    button8 <- mkButton  " " (2,1)
    button9 <- mkButton  " " (3,1)
    button10 <- mkButton " " (4,1)
    button11 <- mkButton " " (0,2)
    button12 <- mkButton " " (1,2)
    button13 <- mkButton " " (2,2)
    button14 <- mkButton " " (3,2)
    button15 <- mkButton " " (4,2)
    button16 <- mkButton " " (0,3)
    button17 <- mkButton " " (1,3)
    button18 <- mkButton " " (2,3)
    button19 <- mkButton " " (3,3)
    button20 <- mkButton " " (4,3)
    button21 <- mkButton " " (0,4)
    button22 <- mkButton " " (1,4)
    button23 <- mkButton " " (2,4)
    button24 <- mkButton " " (3,4)
    button25 <- mkButton " " (4,4)

    topRow   <- newElem "div"
    row1  <- newElem "div"
    row2  <- newElem "div"
    row3  <- newElem "div"
    row4  <- newElem "div"
    row5  <- newElem "div"

    row topRow [actionButton]
    row row1  [button1, button2, button3, button4, button5]
    row row2  [button6, button7, button8, button9, button10]
    row row3  [button11, button12, button13, button14, button15]
    row row4  [button16, button17, button18, button19, button20]
    row row5  [button21, button22, button23, button24, button25]

    let buttons = [topRow, row1, row2, row3, row4, row5]

    column documentBody buttons

    onEvent actionButton Click $ \_ -> do toggleAction actionButton

    onEvent button1 Click $ \_ -> do update button1 field actionButton
    onEvent button2 Click $ \_ -> do update button2 field actionButton
    onEvent button3 Click $ \_ -> do update button3 field actionButton
    onEvent button4 Click $ \_ -> do update button4 field actionButton
    onEvent button5 Click $ \_ -> do update button5 field actionButton
    onEvent button6 Click $ \_ -> do update button6 field actionButton
    onEvent button7 Click $ \_ -> do update button7 field actionButton
    onEvent button8 Click $ \_ -> do update button8 field actionButton
    onEvent button9 Click $ \_ -> do update button9 field actionButton
    onEvent button10 Click $ \_ -> do update button10 field actionButton
    onEvent button11 Click $ \_ -> do update button11 field actionButton
    onEvent button12 Click $ \_ -> do update button12 field actionButton
    onEvent button13 Click $ \_ -> do update button13 field actionButton
    onEvent button14 Click $ \_ -> do update button14 field actionButton
    onEvent button15 Click $ \_ -> do update button15 field actionButton
    onEvent button16 Click $ \_ -> do update button16 field actionButton
    onEvent button17 Click $ \_ -> do update button17 field actionButton
    onEvent button18 Click $ \_ -> do update button18 field actionButton
    onEvent button19 Click $ \_ -> do update button19 field actionButton
    onEvent button20 Click $ \_ -> do update button20 field actionButton
    onEvent button21 Click $ \_ -> do update button21 field actionButton
    onEvent button22 Click $ \_ -> do update button22 field actionButton
    onEvent button23 Click $ \_ -> do update button23 field actionButton
    onEvent button24 Click $ \_ -> do update button24 field actionButton
    onEvent button25 Click $ \_ -> do update button25 field actionButton

toggleAction btn = do
            action <- getProp btn "innerHTML"
            if action == "Press" then do
                set btn [ prop "innerHTML" =: "Flag"]
            else do
                set btn [ prop "innerHTML" =: "Press"]
            return ()


--TODO make a isFlag function
update btn field actionBtn = do
      strPos <- getProp btn "btnID"
      let tile = getElement (toPos strPos) field
      let el = elToString tile
      action <- getProp actionBtn "innerHTML"
      if action == "Flag" then do
        set btn [ prop "innerHTML" =: "F"]
      else do
        if tile == Bomb then do
          set btn [ prop "innerHTML" =: el,  prop "style" =: "width:2em;height:2em"]
          alert "Game Over!"
        else do
          set btn [ prop "innerHTML" =: el,  prop "style" =: "width:2em;height:2em"]
          return ()

--mkButtons :: MineField -> [IO Elem]
--mkButtons field = [mkButton (getElement pos field) pos | pos <- coords ]
--    where coords = makeCoordinates field

elToString :: Tile -> String
elToString (Numeric el) = toString el
elToString _            = "B"

toPos :: String -> Pos
toPos xs = (x,y)
        where x = digitToInt (xs!!1)
              y = digitToInt (xs!!3)

mkButton :: String -> Pos -> IO Elem
mkButton label pos = do
          button <- newElem "button"
          set button [ prop "innerHTML" =: label, prop "btnID" =: show pos, prop "style" =: "width:2em;height:2em"]
          return button
