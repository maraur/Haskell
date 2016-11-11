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
import Test.QuickCheck
import System.Random

card_1 = Card (Numeric 8) Spades
card_2 = Card {rank=King, suit=Hearts}
card_3 = Card {rank=Ace, suit=Hearts}
card_4 = Card {rank=(Numeric 2), suit=Hearts}
card_5 = Card {rank=Ace, suit=Spades}

hand_0 = Empty
hand_1 = Add card_1 Empty
hand_2 = Add card_2 Empty
hand_3 = Add card_3 Empty
hand_4 = Add card_4 hand_3
hand_5 = Add card_5 hand_4


empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank Ace         = 11
valueRank (Numeric n) = n
valueRank _           = 10

valueCard :: Card -> Integer
valueCard c = valueRank(rank c)

numberOfAces :: Hand -> Integer
numberOfAces Empty                            = 0
numberOfAces (Add Card {rank=Ace, suit=s} h)  = 1 + numberOfAces h
numberOfAces (Add c h)                        = numberOfAces h

valueHand :: Hand -> Integer
valueHand Empty     = 0
valueHand (Add c h) = valueCard c + valueHand h

value :: Hand -> Integer
value h | valueHand h > 21  = valueHand h - 10 * numberOfAces h
        | otherwise         = valueHand h

gameOver :: Hand -> Bool
gameOver h | value h > 21 = True
           |otherwise     = False

winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest                    = Bank
                  | gameOver bank                     = Guest
                  | valueHand guest > valueHand bank  = Guest
                  | otherwise                         = Bank

(<+) :: Hand -> Hand -> Hand
Empty <+ h2           = h2
h1 <+ Empty           = h1
(Add card h1) <+ h2   = Add card (h1 <+ h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == size (h1 <+ h2)

--TODO: FIX
rankList :: [Rank]
rankList = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

--TODO: FIX
deckOfSuit :: [Rank] -> Suit -> Hand
--deckOfSuit [] s = Empty
--deckOfSuit (x:xs) s = Add Card {rank=x, suit=s} (deckOfSuit xs s)
deckOfSuit xs s = foldr (\ x -> Add Card{rank = x, suit = s}) Empty xs

--TODO: FIX, use a list as well?
fullDeck :: Hand
fullDeck = deckOfSuit rankList Spades <+ deckOfSuit rankList Hearts
          <+ deckOfSuit rankList Diamonds <+ deckOfSuit rankList Clubs

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _              = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)

--To write this function you will probably need to introduce a help function
--that takes two hands as input, the deck and the bank’s hand.
--To draw a card from the deck you can use where in the following way:
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 15 = playBank' deck' bankHand'
                        | otherwise           = bankHand
  where (deck',bankHand') = draw deck bankHand
--Given a deck, play for the bank according to
--the rules above (starting with an empty hand),
--and return the bank’s final hand:
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

removeCard :: Hand -> Int -> [Card]-> (Card, Hand)
removeCard (Add c1 h1) rIndex cardList
             | length cardList == (rIndex - 1) = (c1, (listToHand (reverse cardList)) <+ h1)
             | otherwise = removeCard h1 rIndex (c1:cardList)

listToHand :: [Card] -> Hand
listToHand cards = foldr (\x -> (Add (x))) Empty cards

--shuffle :: StdGen -> Hand -> Hand
--shuffle S
oneRandomInteger :: StdGen -> Integer
oneRandomInteger g = fst (randomR (0,10) g)
{-
twoRandomIntegers :: StdGen -> (Integer,Integer)
twoRandomIntegers g = (n1, n2)
  where (n1, g1) = randomR (0, 10) g
        (n2, g2) = randomR (0, 10) g1
-}
--prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
--prop_shuffle_sameCards g c h =
--    c `belongsTo` h == c `belongsTo` shuffle g h

--belongsTo :: Card -> Hand -> Bool
--c `belongsTo` Empty = False
--c `belongsTo` (Add c′ h) = c == c′ || c `belongsTo` h

--prop_size_shuffle :: StdGen -> Hand -> Bool
