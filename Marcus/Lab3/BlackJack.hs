{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + Empty
  = 1 + 1 + 0
  = 2
  -}
--------------------------------------------------------------------------
module BlackJack where
import Cards
import RunGame
import System.Random
import Test.QuickCheck hiding (shuffle)

empty :: Hand
empty = Empty
--------------------------------------------------------------------------
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n
valueRank _ = 10
--------------------------------------------------------------------------
valueCard :: Card -> Integer
valueCard c = valueRank(rank c)
--------------------------------------------------------------------------
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
--------------------------------------------------------------------------
gameOver :: Hand -> Bool
gameOver h =  (value h > 21)
--------------------------------------------------------------------------
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest                   = Bank
                  | gameOver bank                    = Guest
                  | valueHand guest > valueHand bank = Guest
                  | otherwise                        = Bank
---------------------------------------------------------------------
(<+) :: Hand -> Hand -> Hand
Empty <+ h2              = h2
h1 <+ Empty              = h1
(Add c1 h1) <+ h2        = (Add c1 (h1 <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size(h1 <+ h2) == (size(h1) + size(h2))
--------------------------------------------------------------------------
fullDeck :: Hand
fullDeck = makeSuit Hearts <+ makeSuit Spades
          <+ makeSuit Diamonds <+ makeSuit Clubs

suits = [Hearts, Spades, Diamonds, Clubs]
ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

makeSuit :: Suit -> Hand
makeSuit suit = foldr (\x -> (Add (Card x suit))) Empty ranks
--------------------------------------------------------------------------
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand        = error "draw: The deck is empty."
draw (Add c deck) hand = (deck, (Add c hand))
--------------------------------------------------------------------------
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
                        | otherwise           = bankHand
  where (deck',bankHand') = draw deck bankHand

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

--------------------------------------------------------------------------

shuffle :: StdGen -> Hand -> Hand
shuffle gen Empty = Empty
shuffle gen hand = (Add newCard (shuffle randGen newDeck))
                  where (randVal, randGen) = (randomR (1, size hand) gen )
                        (newCard, newDeck) = (removeCard hand randVal [] )

removeCard :: Hand -> Int -> [Card]-> (Card, Hand)
removeCard (Add c1 h1) rIndex cardList
             | length cardList == (rIndex - 1) =
                        (c1, (listToHand (reverse cardList)) <+ h1)
             | otherwise = removeCard h1 rIndex (c1:cardList)

listToHand :: [Card] -> Hand
listToHand cards = foldr (\x -> (Add (x))) Empty cards

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = (size h == size (shuffle g h) )

--------------------------------------------------------------------------
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
