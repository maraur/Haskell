{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + Empty
  = 1 + 1 + 0
  = 2
  -}
---------------------------------------------------------
module BlackJack where
import Cards
import RunGame

empty :: Hand
empty = Empty
---------------------------------------------------------

valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank _ = 10

valueCard :: Card -> Integer
valueCard c = valueRank(rank c)

numberOfAces :: Hand -> Integer
numberOfAces Empty                            = 0
numberOfAces (Add Card {rank=Ace, suit=_} h)  = 1 + numberOfAces h
numberOfAces (Add c h)                        = numberOfAces h

--Helper function
valueHand :: Hand -> Integer
valueHand Empty     = 0
valueHand (Add c h) = valueCard c + valueHand h

value :: Hand -> Integer
value h | valueHand h > 21 = valueHand h - 10 * numberOfAces h
        | otherwise        = valueHand h

gameOver :: Hand -> Bool
gameOver h =  (value h > 21)

winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest                   = Bank
                  | gameOver bank                    = Guest
                  | valueHand guest > valueHand bank = Guest
                  | otherwise                        = Bank
