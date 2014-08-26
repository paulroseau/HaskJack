module Cards where

-- Imports

import Control.Monad.State
import Data.Array
import System.Random (mkStdGen
                     , randomR
                     , random
                     , StdGen)

-- Types
data Card = Two | Three | Four | Five |
            Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord)

type Hand = [Card]

newtype Bet = Bet (Hand, Rational) deriving (Show)

data Status = Surrender Rational | Bust Rational | BlackJack Rational |
              Start Bet | Stand Bet | Doubled Bet | Continue Bet 
              deriving (Show)


newtype MyEitherT l m a = MyEitherT { runMyEitherT :: m (Either l a) }

instance Monad m => Monad (MyEitherT l m) where
  return a = MyEitherT ( return $ Right a )
  (MyEitherT x) >>= f = MyEitherT $ x >>= (
                              \either -> case either of
                                Right a -> runMyEitherT $ f a
                                Left l -> return $ Left l -- _ -> x makes the compiler confused with types
                              )

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance Monad m => Monad (MyStateT s m) where
  return a = MyStateT (\s -> return (a, s))
  (MyStateT x) >>= f = MyStateT $ (\s ->
                                  (x s) >>= (\(a, s') -> 
                                            runMyStateT (f a) $ s'))
-----------------------THINKING OUTLOUD-------------------------------
  -- In a way the state is inside the monad m, even though the type is
  -- confusing. The state that matters is the one in (a, s), this is the one on
  -- which you operate. The first state is just the argument, but if you think
  -- of a monad that carries a state, it must have an initial state to start
  -- with. The state will then live "inside" the monad.

  -- Question : why is it not MyStateT { runMyStateT :: m (State s a) }?
  -- I think you could easily write return and >>= that would compile for this
  -- type. However, it doesn't make as much sense as the previous one because it
  -- is not clear to see how you will pass the initial state as a parameter to
  -- the inner (State s a). Think to the case where m is a list... Even if m is
  -- the Maybe monad, you would have to unwrap your maybe to pass the initial
  -- state to the stateful computation, and you lost your context...
----------------------------------------------------------------------

-----------------------THINKING OUTLOUD-------------------------------
-- Target type : [Either String (State StdGen a)] where a = Status
-- Transformers :
-- MyStateT s m1 a = MyStateT { runMyStateT :: s -> m1 (a, s) }
-- MyEitherT l m2 a = MyEitherT { runMyEitherT :: m2 (Either l a) }
-- Rk : the inner most monad has the outer most constructor, since a transformer
-- modifies the inside of a monad. One should read the transformers from right
-- to left.
-- We shoud have :
--  m1 = MyEitherT String [] , which is of kind * -> * 
-- Thinking that (a, s) will be injected in m1 is not a good
-- vision of things, even though it is actually the case in order to construct
-- the function contained in MyStateT. Actually m1 is transformed by MyStateT, and the
-- resulting monad will be injected a. However, MyEitherT [Either l (x, y)] is
-- indeed a monad of base type (x, y), but since this is transformed by a
-- MyStateT, the y is not to be considered as a part of the underlying type of
-- the monad.
-- Thus the target type is actually :
-- MyStateT StdGen (MyEitherT String []) Status
----------------------------------------------------------------------

type Move = Status -> MyStateT StdGen (MyEitherT String []) Status

-- Card logic

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

deck :: [Card]
deck = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

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

bestScore :: Hand -> Int
bestScore cards = case filter (<= 21) $ potentialScores of
                okScores@(_:_) -> maximum okScores
                _                    -> minimum potentialScores
              where potentialScores = scores cards

scores :: Hand -> [Int]
scores cards = foldr (\vs acc -> vs >>= (\v -> fmap (v+) acc)) [0] (map value cards) 

-- Move logic

drawCard :: StdGen -> (Card, StdGen)
drawCard gen = (deck !! randomInt, gen')
                    where (randomInt, gen') = randomR (0, (length deck) - 1) gen

surrender :: Move
surrender (Start (Bet (_, amount)))  = MyStateT (\gen -> MyEitherT [Right (Surrender $ amount / 2, gen)])
surrender status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot surrender after " ++ show status ++ "!"])

stand :: Move
stand (Start b) = MyStateT (\gen -> MyEitherT [Right (Stand b, gen)])
stand (Doubled b) = MyStateT (\gen -> MyEitherT [Right (Stand b, gen)])
stand (Continue b) = MyStateT (\gen -> MyEitherT [Right (Stand b, gen)])
stand status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot stand after " ++ show status ++ "!"])

double :: Move
double (Start (Bet (h, a))) = MyStateT (\gen -> MyEitherT [Right (Doubled $ Bet (h, 2*a), gen)])
double status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot double after " ++ show status ++ "!"])

hit :: Move
hit (Doubled b) = MyStateT $ drawCardAnd Stand b
hit (Start b) = MyStateT $ drawCardAnd Continue b
hit (Continue b) = MyStateT $ drawCardAnd Continue b
hit status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot hit after " ++ show status ++ "!"])

drawCardAnd :: (Bet -> Status) -> Bet -> (StdGen -> MyEitherT String [] (Status, StdGen))
drawCardAnd status (Bet (h, a)) = (\gen -> let (newCard, gen') = drawCard gen 
                                               newHand = newCard:h 
                                           in MyEitherT [Right ( if (bestScore newHand > 21)
                                                                   then Bust a
                                                                   else status (Bet (newHand, a))
                                                                , gen')])

split :: Move
split (Start (Bet ((c1:c2:[]), a)))
  | c1 == c2 = MyStateT (\gen -> 
                            let (newC1, gen') = drawCard gen
                                (newC2, gen'') = drawCard gen'
                                (seed1, gen''') = random gen''
                                (seed2, _) = random gen'''
                            in MyEitherT [Right (Start $ Bet ([c1, newC1], a), mkStdGen seed1),
                                          Right (Start $ Bet ([c2, newC2], a), mkStdGen seed2)])
  | otherwise = MyStateT (\gen -> MyEitherT [Left $ "Error : hand " ++ show [c1, c2] ++ " is not a pair!"])
split status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot split after " ++ show status ++ "!"])

-- Strategies

type Strategy = Array (Int, Card) Move

hardHandBasicStrategy = array ((4, 2), (21, 11)) [
                          ((4, 2), h), ((4, 3), h), ((4, 4), h), ((4, 5), h), ((4, 6), h), ((4, 7), h), ((4, 8), h), ((4, 9), h), ((4, 10), h), ((4, 11), h), 
                          ((5, 2), h), ((5, 3), h), ((5, 4), h), ((5, 5), h), ((5, 6), h), ((5, 7), h), ((5, 8), h), ((5, 9), h), ((5, 10), h), ((5, 11), h), 
                          ((6, 2), h), ((6, 3), h), ((6, 4), h), ((6, 5), h), ((6, 6), h), ((6, 7), h), ((6, 8), h), ((6, 9), h), ((6, 10), h), ((6, 11), h), 
                          ((7, 2), h), ((7, 3), h), ((7, 4), h), ((7, 5), h), ((7, 6), h), ((7, 7), h), ((7, 8), h), ((7, 9), h), ((7, 10), h), ((7, 11), h), 
                          ((8, 2), h), ((8, 3), h), ((8, 4), h), ((8, 5), h), ((8, 6), h), ((8, 7), h), ((8, 8), h), ((8, 9), h), ((8, 10), h), ((8, 11), h), 
                          ((9, 2), h), ((9, 3), dh), ((9, 4), dh), ((9, 5), dh), ((9, 6), dh), ((9, 7), h), ((9, 8), h), ((9, 9), h), ((9, 10), h), ((9, 11), h), 
                          ((10, 2), dh), ((10, 3), dh), ((10, 4), dh), ((10, 5), dh), ((10, 6), dh), ((10, 7), dh), ((10, 8), dh), ((10, 9), dh), ((10, 10), h), ((10, 11), h), 
                          ((11, 2), dh), ((11, 3), dh), ((11, 4), dh), ((11, 5), dh), ((11, 6), dh), ((11, 7), dh), ((11, 8), dh), ((11, 9), dh), ((11, 10), dh), ((11, 11), dh), 
                          ((12, 2), h), ((12, 3), h), ((12, 4), s), ((12, 5), s), ((12, 6), s), ((12, 7), h), ((12, 8), h), ((12, 9), h), ((12, 10), h), ((12, 11), h), 
                          ((13, 2), s), ((13, 3), s), ((13, 4), s), ((13, 5), s), ((13, 6), s), ((13, 7), h), ((13, 8), h), ((13, 9), h), ((13, 10), h), ((13, 11), h), 
                          ((14, 2), s), ((14, 3), s), ((14, 4), s), ((14, 5), s), ((14, 6), s), ((14, 7), h), ((14, 8), h), ((14, 9), h), ((14, 10), h), ((14, 11), h), 
                          ((15, 2), s), ((15, 3), s), ((15, 4), s), ((15, 5), s), ((15, 6), s), ((15, 7), h), ((15, 8), h), ((15, 9), h), ((15, 10), rh), ((15, 11), rh), 
                          ((16, 2), s), ((16, 3), s), ((16, 4), s), ((16, 5), s), ((16, 6), s), ((16, 7), h), ((16, 8), h), ((16, 9), rh), ((16, 10), rh), ((16, 11), rh), 
                          ((17, 2), s), ((17, 3), s), ((17, 4), s), ((17, 5), s), ((17, 6), s), ((17, 7), s), ((17, 8), s), ((17, 9), s), ((17, 10), s), ((17, 11), rs), 
                          ((18, 2), s), ((18, 3), s), ((18, 4), s), ((18, 5), s), ((18, 6), s), ((18, 7), s), ((18, 8), s), ((18, 9), s), ((18, 10), s), ((18, 11), s), 
                          ((19, 2), s), ((19, 3), s), ((19, 4), s), ((19, 5), s), ((19, 6), s), ((19, 7), s), ((19, 8), s), ((19, 9), s), ((19, 10), s), ((19, 11), s), 
                          ((20, 2), s), ((20, 3), s), ((20, 4), s), ((20, 5), s), ((20, 6), s), ((20, 7), s), ((20, 8), s), ((20, 9), s), ((20, 10), s), ((20, 11), s), 
                          ((21, 2), s), ((21, 3), s), ((21, 4), s), ((21, 5), s), ((21, 6), s), ((21, 7), s), ((21, 8), s), ((21, 9), s), ((21, 10), s), ((21, 11), s)]


softHandBasicStrategy = array ((13, 2), (21, 11)) [
                          ((13, 2), h), ((13, 3), h), ((13, 4), h), ((13, 5), dh), ((13, 6), dh), ((13, 7), h), ((13, 8), h), ((13, 9), h), ((13, 10), h), ((13, 11), h), 
                          ((14, 2), h), ((14, 3), h), ((14, 4), h), ((14, 5), dh), ((14, 6), dh), ((14, 7), h), ((14, 8), h), ((14, 9), h), ((14, 10), h), ((14, 11), h), 
                          ((15, 2), h), ((15, 3), h), ((15, 4), dh), ((15, 5), dh), ((15, 6), dh), ((15, 7), h), ((15, 8), h), ((15, 9), h), ((15, 10), h), ((15, 11), h), 
                          ((16, 2), h), ((16, 3), h), ((16, 4), dh), ((16, 5), dh), ((16, 6), dh), ((16, 7), h), ((16, 8), h), ((16, 9), h), ((16, 10), h), ((16, 11), h), 
                          ((17, 2), h), ((17, 3), dh), ((17, 4), dh), ((17, 5), dh), ((17, 6), dh), ((17, 7), h), ((17, 8), h), ((17, 9), h), ((17, 10), h), ((17, 11), h), 
                          ((18, 2), ds), ((18, 3), ds), ((18, 4), ds), ((18, 5), ds), ((18, 6), ds), ((18, 7), s), ((18, 8), s), ((18, 9), h), ((18, 10), h), ((18, 11), h), 
                          ((19, 2), s), ((19, 3), s), ((19, 4), s), ((19, 5), s), ((19, 6), ds), ((19, 7), s), ((19, 8), s), ((19, 9), s), ((19, 10), s), ((19, 11), s), 
                          ((20, 2), s), ((20, 3), s), ((20, 4), s), ((20, 5), s), ((20, 6), s), ((20, 7), s), ((20, 8), s), ((20, 9), s), ((20, 10), s), ((20, 11), s), 
                          ((21, 2), s), ((21, 3), s), ((21, 4), s), ((21, 5), s), ((21, 6), s), ((21, 7), s), ((21, 8), s), ((21, 9), s), ((21, 10), s), ((21, 11), s)]

splitHandBasicStrategy = array ((2, 11), (2, 11)) [
                          ((2, 2), p), ((2, 3), p), ((2, 4), p), ((2, 5), p), ((2, 6), p), ((2, 7), p), ((2, 8), h), ((2, 9), h), ((2, 10), h), ((2, 11), h), 
                          ((3, 2), p), ((3, 3), p), ((3, 4), p), ((3, 5), p), ((3, 6), p), ((3, 7), p), ((3, 8), h), ((3, 9), h), ((3, 10), h), ((3, 11), h), 
                          ((4, 2), h), ((4, 3), h), ((4, 4), h), ((4, 5), p), ((4, 6), p), ((4, 7), h), ((4, 8), h), ((4, 9), h), ((4, 10), h), ((4, 11), h), 
                          ((5, 2), dh), ((5, 3), dh), ((5, 4), dh), ((5, 5), dh), ((5, 6), dh), ((5, 7), dh), ((5, 8), dh), ((5, 9), dh), ((5, 10), h), ((5, 11), h), 
                          ((6, 2), p), ((6, 3), p), ((6, 4), p), ((6, 5), p), ((6, 6), p), ((6, 7), h), ((6, 8), h), ((6, 9), h), ((6, 10), h), ((6, 11), h), 
                          ((7, 2), p), ((7, 3), p), ((7, 4), p), ((7, 5), p), ((7, 6), p), ((7, 7), p), ((7, 8), h), ((7, 9), h), ((7, 10), h), ((7, 11), h), 
                          ((8, 2), p), ((8, 3), p), ((8, 4), p), ((8, 5), p), ((8, 6), p), ((8, 7), p), ((8, 8), p), ((8, 9), p), ((8, 10), p), ((8, 11), rh), -- actually as you split at the start you will always be able to surrender
                          ((9, 2), p), ((9, 3), p), ((9, 4), p), ((9, 5), p), ((9, 6), p), ((9, 7), p), ((9, 8), s), ((9, 9), p), ((9, 10), s), ((9, 11), s), 
                          ((10, 2), s), ((10, 3), s), ((10, 4), s), ((10, 5), s), ((10, 6), s), ((10, 7), s), ((10, 8), s), ((10, 9), s), ((10, 10), s), ((10, 11), s), 
                          ((11, 2), p), ((11, 3), p), ((11, 4), p), ((11, 5), p), ((11, 6), p), ((11, 7), p), ((11, 8), p), ((11, 9), p), ((11, 10), p), ((11, 11), p)] 

  -- Aliases

h :: Move
h = hit
s :: Move
s = stand
p :: Move
p = split
dh :: Move
dh st@(Start _) = double st
dh st = hit st
ds :: Move
ds st@(Start _) = double st
ds st = stand st
rh :: Move
rh st@(Start _) = surrender st
rh st = hit st
rs :: Move
rs st@(Start _) = surrender st
rs st = stand st

-- Tests
hand1 = [Ace, Two]
hand2 = [Queen, King]
hand3 = [Four, Three]
hand4 = [King, King]

bet1 = Bet (hand1, 5)
bet2 = Bet (hand2, 5)
bet3 = Bet (hand3, 5)
bet4 = Bet (hand4, 5)

initialStatus1 = Start bet1
initialStatus2 = Start bet2
initialStatus3 = Start bet3
initialStatus4 = Start bet4

result1 :: MyStateT StdGen (MyEitherT String []) Status
result1 = return initialStatus1 >>= surrender

result2 :: MyStateT StdGen (MyEitherT String []) Status
result2 = return initialStatus2 >>= double >>= stand

result3 :: MyStateT StdGen (MyEitherT String []) Status
result3 = return initialStatus3 >>= hit >>= hit >>= hit

result4 :: MyStateT StdGen (MyEitherT String []) Status
result4 = return initialStatus4 >>= split 
