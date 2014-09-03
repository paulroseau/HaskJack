{-# LANGUAGE TypeSynonymInstances #-}

module Commons where

import Data.Monoid

-- Constants
maxNbSplit = 4 :: Int
blackJackQuote = 3 / 2 :: Amount

-- Custom Types
type Amount = Double
instance Monoid Amount where
  mempty = 0
  mappend = (+)

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
