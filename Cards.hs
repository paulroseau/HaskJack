module Cards 
  (
    Card
    , Hand
    , isBlackJack
    , isSoftHand
    , isPair
    , drawCard
    , drawHand
    , bestScore
  )
where

import System.Random (randomR
                     , StdGen)

data Card = Two | Three | Four | Five |
            Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord)

type Hand = [Card]

isBlackJack :: Hand -> Bool
isBlackJack h = (length h == 2) && 
                (any ((==) Ace) h) && 
                (
                  (any ((==) Ten) h) || 
                  (any ((==) Jack) h) ||
                  (any ((==) Queen) h) ||
                  (any ((==) King) h)
                )

isSoftHand :: Hand -> Bool
isSoftHand cards = (length . scores $ cards) >= 2

isPair :: Hand -> Bool
isPair (c1:c2:[]) = c1 == c2
isPair _ = False

drawCard :: StdGen -> (Card, StdGen)
drawCard gen = (deck !! randomInt, gen')
                    where (randomInt, gen') = randomR (0, (length deck) - 1) gen

drawHand :: StdGen -> (Hand, StdGen)
drawHand gen = let (c1, gen') = drawCard gen
                   (c2, gen'') = drawCard gen'
               in ([c1, c2], gen'')

bestScore :: Hand -> Int
bestScore cards = if null okScores 
                    then maximum okScores
                    else minimum potentialScores
                  where potentialScores = scores cards
                        okScores = filter (<= 21) potentialScores

scores :: Hand -> [Int]
scores cards = foldr (\vs acc -> vs >>= (\v -> fmap (v+) acc)) [0] (map value cards) 

value :: Card -> [Int]
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
value Ace = [1, 11]

deck = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
deck :: [Card]

-- Idea of tests : isSoftHand => Ace and contraposee
