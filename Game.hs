module Game 
  (
    Move
    , Status (..)
    , Bet (..)
    , Amount
    , NumberOfSplits
    , surrender
    , stand
    , double
    , hit
    , split
  )
where

import Cards
import Commons
import System.Random (StdGen)

type Move = Status -> MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]

data Status = Surrender Amount | Bust Amount | BlackJack Amount |
              Start Bet | StartNoSplit Bet | StartSplit Bet | 
              Stand Bet | Doubled Bet | Continue Bet 
              deriving (Show)

newtype Bet = Bet (Hand, Amount) deriving (Show)

type NumberOfSplits = Int

surrender :: Move
surrender (Start (Bet (_, a))) = return [Surrender a]
surrender (StartSplit (Bet (_, a))) = return [Surrender a]
surrender (StartNoSplit (Bet (_, a))) = return [Surrender a]
surrender status = MyStateT (\(gen, balance, nbSplit) -> Left ("Error : cannot surrender after " ++ show status ++ "!"))

stand :: Move
stand (Start b) = return [Stand b]
stand (StartSplit b) = return [Stand b]
stand (StartNoSplit b) = return [Stand b]
stand (Doubled b) = return [Stand b]
stand (Continue b) = return [Stand b]
stand status = MyStateT (\(gen, balance, nbSplit) -> Left ("Error : cannot stand after " ++ show status ++ "!"))

double :: Move
double (Start (Bet (h, a))) = double' h a
double (StartSplit (Bet (h, a))) = double' h a
double (StartNoSplit (Bet (h, a))) = double' h a
double status = MyStateT (\(gen, balance, nbSplit) -> Left ("Error : cannot double after " ++ show status ++ "!"))

double' :: Hand -> Amount -> MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
double' h a = MyStateT (\(gen, balance, nbSplit) -> 
                if (balance - a >= 0)
                  then Right $ ([Doubled $ Bet (h, 2*a)], (gen, balance - a, nbSplit))
                  else Right $ ([Continue $ Bet (h, a)], (gen, balance, nbSplit)))

hit :: Move
hit (Doubled b) = drawCardAnd Stand b
hit (Start b) = drawCardAnd Continue b
hit (StartSplit b) = drawCardAnd Continue b
hit (StartNoSplit b) = drawCardAnd Continue b
hit (Continue b) = drawCardAnd Continue b
hit status = MyStateT (\(gen, balance, nbSplit) -> Left ("Error : cannot hit after " ++ show status ++ "!"))

drawCardAnd :: (Bet -> Status) -> Bet -> MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
drawCardAnd status (Bet (h, a)) = MyStateT (\(gen, balance, nbSplit) -> 
                                    let (newCard, gen') = drawCard gen 
                                        newHand = newCard:h 
                                    in Right $ ([if (bestScore newHand > 21) 
                                                   then Bust a 
                                                   else status (Bet (newHand, a))]
                                               , (gen', balance, nbSplit))) 

split :: Move
split (Start (Bet ((c1:c2:[]), a))) = split' c1 c2 a
split (StartSplit (Bet ((c1:c2:[]), a))) = split' c1 c2 a
split status = MyStateT (\(gen, balance, nbSplit) -> Left ("Error : cannot split after " ++ show status ++ "!"))

split' :: Card -> Card -> Amount -> MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
split' c1 c2 a 
  | c1 == c2 = MyStateT (\(gen, balance, nbSplit) -> 
                 if (balance - a >= 0 && nbSplit < maxNbSplit)
                   then let (newC1, gen') = drawCard gen
                            (newC2, gen'') = drawCard gen'
                        in Right $ ([StartSplit $ Bet ([c1, newC1], a), StartSplit $ Bet ([c2, newC2], a)], (gen'', balance - a, nbSplit + 1))
                   else Right $ ([StartNoSplit $ Bet ([c1, c2], a)], (gen, balance, nbSplit)))
  | otherwise = MyStateT (\(gen, balance, nbSplit) -> Left ("Error : hand " ++ show [c1, c2] ++ " is not a pair!"))
