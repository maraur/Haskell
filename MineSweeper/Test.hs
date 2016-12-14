import Haste
import Haste.DOM
import Haste.Events
import MineSweeper
import System.Random
import WebFudgets
import Data.Maybe
import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)
import Foreign.Marshal
import System.IO.Unsafe

import Pages

--TODO The buttons jump when pressed



main = do
    --Create a randomized minefield
    g <- newStdGen
    let field = calculateField (makeBombField 7 (makeEmptyField 5 5) g)
    --let field = makeGuiField field
    --let coords = makeCoordinates field
    --let buttons = mkButtons field

    -- Elements
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

    --Layout
    topRow    <- newElem "div"
    row1      <- newElem "div"
    row2      <- newElem "div"
    row3      <- newElem "div"
    row4      <- newElem "div"
    row5      <- newElem "div"

    row topRow [actionButton]
    row row1  [button1, button2, button3, button4, button5]
    row row2  [button6, button7, button8, button9, button10]
    row row3  [button11, button12, button13, button14, button15]
    row row4  [button16, button17, button18, button19, button20]
    row row5  [button21, button22, button23, button24, button25]

    let buttons = [topRow, row1, row2, row3, row4, row5]

    --column documentBody ((mkButtons field))
    column documentBody buttons

    let buttons' = [(\x y -> x++y) "button" (show x) | x<-[1..25]]

    --map setEvent buttons'

    --Interaction
    onEvent actionButton Click $ \_ -> do toggleAction actionButton

    --let setEvent btn = do onEvent btn Click $ \_ -> do gameLoop btn field actionButton

{-
    onEvent button1 Click $ \_ -> do gameLoop button1 field actionButton
    onEvent button2 Click $ \_ -> do gameLoop button2 field actionButton
    onEvent button3 Click $ \_ -> do gameLoop button3 field actionButton
    onEvent button4 Click $ \_ -> do gameLoop button4 field actionButton
    onEvent button5 Click $ \_ -> do gameLoop button5 field actionButton
    onEvent button6 Click $ \_ -> do gameLoop button6 field actionButton
    onEvent button7 Click $ \_ -> do gameLoop button7 field actionButton
    onEvent button8 Click $ \_ -> do gameLoop button8 field actionButton
    onEvent button9 Click $ \_ -> do gameLoop button9 field actionButton
    onEvent button10 Click $ \_ -> do gameLoop button10 field actionButton
    onEvent button11 Click $ \_ -> do gameLoop button11 field actionButton
    onEvent button12 Click $ \_ -> do gameLoop button12 field actionButton
    onEvent button13 Click $ \_ -> do gameLoop button13 field actionButton
    onEvent button14 Click $ \_ -> do gameLoop button14 field actionButton
    onEvent button15 Click $ \_ -> do gameLoop button15 field actionButton
    onEvent button16 Click $ \_ -> do gameLoop button16 field actionButton
    onEvent button17 Click $ \_ -> do gameLoop button17 field actionButton
    onEvent button18 Click $ \_ -> do gameLoop button18 field actionButton
    onEvent button19 Click $ \_ -> do gameLoop button19 field actionButton
    onEvent button20 Click $ \_ -> do gameLoop button20 field actionButton
    onEvent button21 Click $ \_ -> do gameLoop button21 field actionButton
    onEvent button22 Click $ \_ -> do gameLoop button22 field actionButton
    onEvent button23 Click $ \_ -> do gameLoop button23 field actionButton
    onEvent button24 Click $ \_ -> do gameLoop button24 field actionButton
    onEvent button25 Click $ \_ -> do gameLoop button25 field actionButton
-}
-------------------------------------------------------------------------------
toggleAction btn = do
            action <- getProp btn "innerHTML"
            if action == "Press" then do
                set btn [ prop "innerHTML" =: "Flag"]
            else do
                set btn [ prop "innerHTML" =: "Press"]
            return ()


--TODO count down on flags/bombs?
--TODO when you press a tile with 0 it should reveal more buttons
--    (probably needs a bitter field for 0 to come up though :(  )
gameLoop btn field actionBtn = do
      strPos <- getProp btn "btnID"
      let pos = toPos strPos
      let tile = getElement pos field
      let el = tileToString tile
      action <- getProp actionBtn "innerHTML"
      if isFlag action then do
        set btn [ prop "innerHTML" =: "F"]
      else do
        if isBomb tile then do
          setLabel btn el
          alert "Game Over!"
        else do
          setLabel btn el
          return ()

--TODO Doesn't work
{-
isGameWon' field buttons = buttonsPressed == shouldBePressed
      where shouldBePressed = getNonBombsPos field
            coords          = makeCoordinates field
            buttonsPressed  = [toPos (getProp x "btnID") | x <- buttons, getProp x "status" == "visible"]
-}

setLabel btn label =
      set btn [ prop "innerHTML" =: label,
                prop "style" =: "width:2em;height:2em",
                prop "status" =: "visible"]

mkButtons :: MineField -> [ Elem]
mkButtons field = [unsafePerformIO (mkButtons' field pos) | pos <- coords ]
    where coords = makeCoordinates field

mkButtons' field pos = do button <- mkButton (tileToString (getElement pos field)) pos
                          return button

isFlag :: String -> Bool
isFlag xs = xs == "Flag"

tileToString :: Tile -> String
tileToString (Numeric el) = toString el
tileToString _            = "B"

toPos :: String -> Pos
toPos xs = (x,y)
        where x = digitToInt (xs!!1)
              y = digitToInt (xs!!3)

mkButton :: String -> Pos -> IO Elem
mkButton label pos = do
          button <- newElem "button"
          set button [  prop "innerHTML" =: label,
                        prop "btnID" =: show pos,
                        prop "style" =: "width:2em;height:2em",
                        prop "status" =: "hidden"]
          return button
