module Commons where

-- Constants
maxNbSplit = 4 :: Int
blackJackQuote = 3 / 2 :: Amount

-- Personal Monads
newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance Monad m => Monad (MyStateT s m) where
  return a = MyStateT (\s -> return (a, s))
  (MyStateT x) >>= f = MyStateT $ (\s ->
                                  (x s) >>= (\(a, s') -> 
                                            runMyStateT (f a) $ s'))

newtype MyWriterT w m a = MyWriterT { runMyWriterT :: m (a, w) }

instance Monad m, Monoid w => Monad (MyWriterT w m) where
  return a = MyWriterT (return a, mempty)
  (MyWriterT (x, w)) >>= f = let (y, w') = runMyWriterT (f x) 
                             in MyWriterT (y, w `mappend` w')
