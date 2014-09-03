-- Tests
hand1 = [Ace, Two]
hand2 = [Queen, King]
hand3 = [Four, Three]
hand4 = [King, King]

bet1 = Bet (hand1, 5)
bet2 = Bet (hand2, 5)
bet3 = Bet (hand3, 5)
bet4 = Bet (hand4, 5)

initialStatus1 = [Start bet1]
initialStatus2 = [Start bet2]
initialStatus3 = [Start bet3]
initialStatus4 = [Start bet4]

result1 :: MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
result1 = return initialStatus1 >>= unsafeSurrender
          where unsafeSurrender (c:[]) = surrender c
                unsafeSurrender _ = undefined

result2 :: MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
result2 = return initialStatus2 >>= unsafeDouble >>= unsafeStand
          where unsafeDouble (c:[]) = double c
                unsafeDouble _ = undefined
                unsafeStand (c:[]) = stand c
                unsafeStand _ = undefined

result3 :: MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
result3 = return initialStatus3 >>= unsafeHit >>= unsafeHit >>= unsafeHit
          where unsafeHit (c:[]) = hit c
                unsafeHit _ = undefined

result4 :: MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
result4 = return initialStatus4 >>= unsafeSplit 
          where unsafeSplit (c:[]) = split c
                unsafeSplit _ = undefined
