Need package, install with:
cabal install random-shuffle

use with:
shuffle' list (length list) StdGen
^ doesn't seem to return new StdGen? but doesn't matter in our case

Link:
https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html


----------------------------------------------------------------------------------------------
TODO!!!

-Input is done with a unsafe conversion that needs to be changed....
-There needs to be a check if the game is won.

- Make sure first click is never a bomb?
- Use GuiMineField everywhere

- remove everything in the bottom of MineSweeper.hs, it's mostly debug stuff
----------------------------------------------------------------------------------------------
