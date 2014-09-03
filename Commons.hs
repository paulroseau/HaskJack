module Commons where

-- Constants
maxNbSplit = 4 :: Int
blackJackQuote = 3 / 2 :: Rational
mustHitSoftSeventeen = True

-- Personal Monads
newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance Monad m => Monad (MyStateT s m) where
  return a = MyStateT (\s -> return (a, s))
  (MyStateT x) >>= f = MyStateT $ (\s ->
                                  (x s) >>= (\(a, s') -> 
                                            runMyStateT (f a) $ s'))
