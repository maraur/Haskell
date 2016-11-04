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
--value :: Hand -> Integer

--valueRank :: Rank -> Integer

--valueCard :: Card ->

example_card_1 = Card King Clubs

example_card_2 = Card {rank=Ace, suit=Spades}

example_hand_0 = Empty

example_hand_1 = Add example_card_1 Empty

example_hand_2 = Add example_card_2 example_hand_1

example_hand_3 = Add example_card_2 example_hand_2

example_hand_4 = Add example_card_1 example_hand_3

example_hand_5 = Add example_card_1 example_hand_4

example_hand_6 = Add example_card_2 example_hand_5

example_hand_7 = Add example_card_2 example_hand_6

numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add c h) | rank c == Ace = 1 + numberOfAces h
                       | otherwise     = numberOfAces h

--gameOver :: Hand -> Bool

--winner :: Hand -> Hand -> Player
