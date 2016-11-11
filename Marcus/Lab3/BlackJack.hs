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
import System.Random
import Test.QuickCheck

empty :: Hand
empty = Empty
---------------------------------------------------------

card_1 = Card (Numeric 8) Spades
card_2 = Card {rank=King, suit=Hearts}
card_3 = Card {rank=Ace, suit=Hearts}
card_4 = Card {rank=(Numeric 2), suit=Hearts}
card_5 = Card {rank=Ace, suit=Spades}

hand_0 = Empty
hand_1 = Add card_1 Empty
hand_2 = Add card_2 hand_1
hand_3 = Add card_3 hand_2
hand_4 = Add card_4 hand_3
hand_5 = Add card_5 hand_4


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

(<+) :: Hand -> Hand -> Hand
Empty <+ h2              = h2
h1 <+ Empty              = h1
(Add c1 h1) <+ h2        = (Add c1 (h1 <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size(h1 <+ h2) == (size(h1) + size(h2))


fullDeck :: Hand
--fullDeck = foldr (\x -> makeSuit x (<+)) [] suits
fullDeck = makeSuit Hearts <+ makeSuit Spades <+ makeSuit Diamonds
          <+ makeSuit Clubs

suits = [Hearts, Spades, Diamonds, Clubs]
ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

makeSuit :: Suit -> Hand
makeSuit suit = foldr (\x -> (Add (Card x suit))) Empty ranks

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand        = error "draw: The deck is empty."
draw (Add c deck) hand = (deck, (Add c hand))
{-
Deck -> Hand
If the deck is empty, report an error using error:
error "draw: The deck is empty."
-}

-- playBank :: Hand -> Hand

-- shuffle :: StdGen -> Hand -> Hand

-- prop_size_shuffle :: StdGen -> Hand -> Bool
