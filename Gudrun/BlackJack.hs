{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + Empty
  = 1 + 1 + 0
  = 2
  -}

module BlackJack where
import Cards
import RunGame

empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank _ = 10

valueCard :: Card -> Integer
valueCard c = valueRank(rank c)

numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add c h) | rank c == Ace = 1 + numberOfAces h
                       | otherwise     = numberOfAces h

valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add c h) = valueCard c + valueHand h

value :: Hand -> Integer
value h | valueHand h > 21 = valueHand h - 10 * numberOfAces h
        | otherwise = valueHand h

gameOver :: Hand -> Bool
gameOver h | value h > 21 = True
           |otherwise = False

winner :: Hand -> Hand -> Player
winner h_guest h_bank | value h_guest > 21 = Bank
                      | value h_bank > 21 = Guest
                      | value h_guest > value h_bank = Guest
                      | otherwise = Bank
