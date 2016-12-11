module RunGame where

import Data.Char
import System.Random
import MineSweeper

--TODO Fix this
{-
-- | The interface to the game model
data Interface = Interface
  { iEmpty    :: Hand
  , iFullDeck :: Hand
  , iValue    :: Hand -> Integer
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Hand -> Hand -> (Hand, Hand)
  , iPlayBank :: Hand -> Hand
  , iShuffle  :: StdGen -> Hand -> Hand
  }
-}

-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  g <- newStdGen
  gameLoop i (makeField 10 10 10 g)

-- | Play until the guest player is bust or chooses to stop.
gameLoop :: Interface -> MineField -> IO ()
gameLoop i field = do
  if iGameOver i guest then do
    finish i deck guest
   else do
    putStrLn "Draw another card? [y]"
    yn <- getLine
    if null yn || not (map toLower yn == "n") then do
      let (deck', guest') = iDraw i deck guest
      gameLoop i deck' guest'
     else
      finish i deck guest

-- | Display the bank's final score and the winner.
finish :: Interface -> Hand -> Hand -> IO ()
finish i deck guest = do
  putStrLn ("The bank's final score: " ++ show (iValue i bank))
  putStrLn ("Winner: " ++ show (iWinner i guest bank))
  where
  bank = iPlayBank i deck
