{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + Empty
  = 1 + 1 + 0
  = 2
  -}
-----------------------------------------------------------------------------
module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random

empty :: Hand
empty = Empty

-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
gameOver :: Hand -> Bool
gameOver h | value h > 21 = True
           |otherwise     = False

-----------------------------------------------------------------------------
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest                    = Bank
                  | gameOver bank                     = Guest
                  | valueHand guest > valueHand bank  = Guest
                  | otherwise                         = Bank

-----------------------------------------------------------------------------
(<+) :: Hand -> Hand -> Hand
Empty <+ h2           = h2
h1 <+ Empty           = h1
(Add card h1) <+ h2   = Add card (h1 <+ h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == size (h1 <+ h2)

-----------------------------------------------------------------------------
rankList :: [Rank]
rankList = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

deckOfSuit :: [Rank] -> Suit -> Hand
deckOfSuit xs s = foldr (\ x -> Add Card{rank = x, suit = s}) Empty xs

fullDeck :: Hand
fullDeck = deckOfSuit rankList Spades <+ deckOfSuit rankList Hearts
          <+ deckOfSuit rankList Diamonds <+ deckOfSuit rankList Clubs

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _              = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)

-----------------------------------------------------------------------------
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
                        | otherwise           = bankHand
  where (deck',bankHand') = draw deck bankHand

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-----------------------------------------------------------------------------
removeCard :: Hand ->  Hand -> Integer -> (Card, Hand)
removeCard (Add c h1) h2 1        = (c, h1 <+ h2)
removeCard h1         h2 index   = removeCard h1' h2' (index-1)
  where (h1', h2') = draw h1 h2

shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g h     = Add card' (shuffle g1 hand')
  where (card', hand')  = removeCard h Empty index
        (index, g1)     = randomR (1, size h) g

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

-----------------------------------------------------------------------------
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation
