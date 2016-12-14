module RunGame where

import Data.Char
import System.Random
import MineSweeper
import System.IO.Unsafe


-- | Runs the game
runGame :: IO ()
runGame = do
  putStrLn "Welcome to Mine Sweeper."
  g <- newStdGen
  let bombField = makeField 10 10 10 g
  gameLoop (makeGuiField bombField)

-- | Play until the game is over or won
gameLoop :: GuiMineField -> IO ()
gameLoop field =
  if isGameOver field then do
       putStrLn "Game over!"
       else do
         if isGameWon field then
            putStrLn "You win!"
            else do
              printGuiField field
              putStrLn "Press f to flag a tile, any other input will lead to showing a tile"
              choice <- getLine
              let updatePos = readCoordinate
              if null choice || (map toLower choice /= "f") then do
                let field' = revealTile field (unsafePerformIO updatePos)
                gameLoop field'
                else do
                  let field' = flagTile field (unsafePerformIO updatePos)
                  gameLoop field'

-- Read coordinate from user input
readCoordinate :: IO Pos
readCoordinate = do
  putStrLn "Enter x coordinate"
  x <- getLine
  putStrLn "Enter y coordinate"
  y <- getLine
  return ((read x :: Int),(read y :: Int))
