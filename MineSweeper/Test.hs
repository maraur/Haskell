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

    let buttons' = mkButtons field
    --column documentBody buttons'
    column documentBody buttons

    --let buttons' = [(\x y -> x++y) "button" (show x) | x<-[1..25]]

    --map setEvent buttons'

    --Interaction
    onEvent actionButton Click $ \_ -> toggleAction actionButton

    --let setEvent btn = do onEvent btn Click $ \_ -> do gameLoop btn field actionButton


    onEvent button1 Click $ \_ -> gameLoop button1 field actionButton buttons'
    onEvent button2 Click $ \_ -> gameLoop button2 field actionButton buttons'
    onEvent button3 Click $ \_ -> gameLoop button3 field actionButton buttons'
    onEvent button4 Click $ \_ -> gameLoop button4 field actionButton buttons'
    onEvent button5 Click $ \_ -> gameLoop button5 field actionButton buttons'
    onEvent button6 Click $ \_ -> gameLoop button6 field actionButton buttons'
    onEvent button7 Click $ \_ -> gameLoop button7 field actionButton buttons'
    onEvent button8 Click $ \_ -> gameLoop button8 field actionButton buttons'
    onEvent button9 Click $ \_ -> gameLoop button9 field actionButton buttons'
    onEvent button10 Click $ \_ -> gameLoop button10 field actionButton buttons'
    onEvent button11 Click $ \_ -> gameLoop button11 field actionButton buttons'
    onEvent button12 Click $ \_ -> gameLoop button12 field actionButton buttons'
    onEvent button13 Click $ \_ -> gameLoop button13 field actionButton buttons'
    onEvent button14 Click $ \_ -> gameLoop button14 field actionButton buttons'
    onEvent button15 Click $ \_ -> gameLoop button15 field actionButton buttons'
    onEvent button16 Click $ \_ -> gameLoop button16 field actionButton buttons'
    onEvent button17 Click $ \_ -> gameLoop button17 field actionButton buttons'
    onEvent button18 Click $ \_ -> gameLoop button18 field actionButton buttons'
    onEvent button19 Click $ \_ -> gameLoop button19 field actionButton buttons'
    onEvent button20 Click $ \_ -> gameLoop button20 field actionButton buttons'
    onEvent button21 Click $ \_ -> gameLoop button21 field actionButton buttons'
    onEvent button22 Click $ \_ -> gameLoop button22 field actionButton buttons'
    onEvent button23 Click $ \_ -> gameLoop button23 field actionButton buttons'
    onEvent button24 Click $ \_ -> gameLoop button24 field actionButton buttons'
    onEvent button25 Click $ \_ -> gameLoop button25 field actionButton buttons'

-------------------------------------------------------------------------------
toggleAction btn = do
            action <- getProp btn "innerHTML"
            if action == "Press" then
                set btn [ prop "innerHTML" =: "Flag"]
            else
                set btn [ prop "innerHTML" =: "Press"]
            return ()


--TODO count down on flags/bombs?
--TODO when you press a tile with 0 it should reveal more buttons
--    (probably needs a bitter field for 0 to come up though :(  )
gameLoop btn field actionBtn buttons' = do
      strPos <- getProp btn "pos"
      let pos = toPos strPos
      let tile = getElement pos field
      let el = tileToString tile
      action <- getProp actionBtn "innerHTML"
      if isFlag action then
        set btn [prop "innerHTML" =: "F"]
      else
        if isBomb tile then do
          updateButton btn el "hidden"
          alert "Game Over!"
        else do
          updateButton btn el "visible"
          if isGameWon' field buttons' then
            alert "Congratulations, you won!"
          else
            return ()

--TODO Doesn't work
isGameWon' :: MineField -> [Elem]-> Bool
isGameWon' field buttons'= shouldBePressed == buttonsPressed
      where shouldBePressed = getNonBombsPos field
            coords          = makeCoordinates field
            buttonsPressed  = [toPos(unsafePerformIO (getProp x "pos")) |
                                  x <- buttons',
                                  unsafePerformIO (getProp x "status") == "visible"]

updateButton btn label status =
      set btn [ prop "innerHTML"  =: label,
                prop "style"      =: "width:2em;height:2em",
                prop "status"     =: status]

mkButtons :: MineField -> [Elem]
mkButtons field = [unsafePerformIO (mkButtons' pos) | pos <- coords ]
    where coords = makeCoordinates field

mkButtons' =  mkButton " "


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
                        prop "pos" =: show pos,
                        prop "style" =: "width:2em;height:2em",
                        prop "status" =: "hidden"]
          return button
