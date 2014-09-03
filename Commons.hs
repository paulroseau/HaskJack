{-# LANGUAGE TypeSynonymInstances #-}

module Commons where

import Data.Monoid

-- Constants
maxNbSplit = 4 :: Int
blackJackQuote = 3 / 2 :: Double

-- Custom Types
newtype Amount = Amount { getAmount :: Double } deriving (Show, Eq, Ord)

instance Num Amount where
  (+) (Amount a) (Amount b) = Amount (a + b)
  (-) (Amount a) (Amount b) = Amount (a - b)
  (*) (Amount a) (Amount b) = Amount (a * b)
  abs (Amount x) = Amount (abs x)
  signum (Amount x) = Amount (signum x)
  fromInteger x = Amount (fromInteger x)

instance Monoid Amount where
  mempty = Amount 0
  mappend (Amount a) (Amount b) = Amount (a + b)

-- Personal Monad Transformers even though they already exist in mtl
newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance Monad m => Monad (MyStateT s m) where
  return a = MyStateT (\s -> return (a, s))
  (MyStateT x) >>= f = MyStateT $ (\s ->
                                  (x s) >>= (\(a, s') -> 
                                            runMyStateT (f a) $ s'))

newtype MyWriterT w m a = MyWriterT { runMyWriterT :: m (a, w) }

instance (Monad m, Monoid w) => Monad (MyWriterT w m) where
  return a = MyWriterT (return (a, mempty))
  (MyWriterT x) >>= f = MyWriterT (x >>= (\(a, w) -> runMyWriterT (f a) >>= (\(a', w') -> return (a', w `mappend` w'))))
