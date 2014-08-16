module Cards where

import Control.Monad.State
import System.Random
import Data.Tuple (fst, snd, swap)

data Card = Ace | Two | Three | Four |
            Five | Six | Seven | Eight |
            Nine | Ten | Jack | Queen | King
            deriving (Show)

type Hand = [Card]

newtype Bet = Bet (Hand, Int) deriving (Show)

value :: Card -> [Int]
value Ace = [1, 11]
value Two = [2]
value Three = [3]
value Four = [4]
value Five = [5]
value Six = [6]
value Seven = [7]
value Eight = [8]
value Nine = [9]
value Ten = [10]
value Jack = [10]
value Queen = [10]
value King = [10]

deck :: [Card]
deck = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

score :: Hand -> [Int]
score cards = foldr (\vs acc -> vs >>= (\v -> fmap (v+) acc)) [0] (map value cards)

drawCard :: State StdGen Card
drawCard = get >>= 
           (\generator -> 
           let (randomInt, newGenerator) = randomR (0, (lenght deck) - 1) generator
           in put newGenerator >> return (deck !! randomInt)) 


stand :: Bet -> Bet
stand = id

hit :: Card -> Bet -> Bet
hit c (Bet (cs, amount)) = Bet (c:cs, amount)

--double 


-- todo
-- "hit" is just a function which appends a new card to the player's hand
-- "stay" is just returning the player's hand
--
-- a player has a certain amount of money
-- a player is playing with a [(Hand, Amount)], each decision affects either the
-- hand or the amount, it's a list for doubling.
