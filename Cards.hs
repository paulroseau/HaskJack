module Cards where

-- Imports

import System.Random
import Control.Monad.State

-- Types
data Card = Ace | Two | Three | Four |
            Five | Six | Seven | Eight |
            Nine | Ten | Jack | Queen | King
            deriving (Show, Eq)

type Hand = [Card]

newtype Bet = Bet (Hand, Rational) deriving (Show)

data Status = Surrender Rational | Bust Rational | BlackJack Rational |
              Stand Bet | Doubled Bet | Continue Bet 
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
                                  (x s) >>= (\(a, s') -> runMyStateT (f a) $ s')
                              )
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
-- resulting monad will be injected a. However, MyEitherT [Either l (x, y)] is indeed a monad of base type (x, y), but since this is transformed by a MyStateT, the y is not to be considered as a part of the underlying type of the monad.
-- Thus the target type is actually :
-- MyStateT StdGen (MyEitherT String []) Status
----------------------------------------------------------------------

type Move = Status -> MyStateT StdGen (MyEitherT String []) Status

-- Card logic

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

isBlackJack :: Hand -> Bool
isBlackJack h = (length h == 2) && 
                (any ((==) Ace) h) && 
                (
                  (any ((==) Ten) h) || 
                  (any ((==) Jack) h) ||
                  (any ((==) Queen) h) ||
                  (any ((==) King) h)
                )

score :: Hand -> Int
score cards = case filter (<= 21) $ potentialScores of
                filteredScores@(_:_) -> maximum filteredScores
                _                    -> minimum potentialScores
              where potentialScores = foldr (\vs acc -> vs >>= (\v -> fmap (v+) acc)) [0] (map value cards)

-- Move logic

drawCard :: StdGen -> (Card, StdGen)
drawCard generator = (deck !! randomInt, newGenerator)
                    where (randomInt, newGenerator) = randomR (0, (length deck) - 1) generator

surrender :: Move
surrender (Continue (Bet (_, amount)))  = MyStateT (\gen -> MyEitherT [Right (Surrender $ amount / 2, gen)])
surrender status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot surrender after " ++ show status ++ "!"])

stand :: Move
stand (Doubled b) = MyStateT (\gen -> MyEitherT [Right (Stand b, gen)])
stand (Continue b) = MyStateT (\gen -> MyEitherT [Right (Stand b, gen)])
stand status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot stand after " ++ show status ++ "!"])

double :: Move
double (Continue (Bet (h, a))) = MyStateT (\gen -> MyEitherT [Right (Doubled $ Bet (h, 2*a), gen)])
double status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot double after " ++ show status ++ "!"])

hit :: Move
hit (Doubled (Bet (h, a))) = 
  MyStateT (\gen -> 
    let (newCard, newGenerator) = drawCard gen in
      let newHand = newCard:h in
        MyEitherT [Right 
          (if (score newHand > 21)
            then Bust a
            else Stand (Bet (newHand, a))
          , newGenerator)])
hit (Continue (Bet (h, a))) = 
  MyStateT (\gen -> 
    let (newCard, newGenerator) = drawCard gen in
      let newHand = newCard:h in
        MyEitherT [Right 
          (if (score newHand > 21)
            then Bust a
            else Continue (Bet (newHand, a))
          , newGenerator)])
hit status = MyStateT (\gen -> MyEitherT [Left $ "Error : cannot hit after " ++ show status ++ "!"])

-- split :: Move
-- split = undefined

-- Tests
hand1 = [Ace, Two]
hand2 = [Queen, King]
hand3 = [Four, Three]

bet1 = Bet (hand1, 5)
bet2 = Bet (hand2, 5)
bet3 = Bet (hand3, 5)

initialStatus1 = Continue bet1
initialStatus2 = Continue bet2
initialStatus3 = Continue bet3

result1 :: MyStateT StdGen (MyEitherT String []) Status
result1 = return initialStatus1 >>= surrender

result2 :: MyStateT StdGen (MyEitherT String []) Status
result2 = return initialStatus2 >>= double >>= stand

result3 :: MyStateT StdGen (MyEitherT String []) Status
result3 = return initialStatus3 >>= hit >>= hit >>= hit
