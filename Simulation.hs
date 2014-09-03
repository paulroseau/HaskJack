module Simulation where

import Cards
import Commons
import Control.Monad.State
import Game
import Strategy
import System.Random (StdGen)

simulate :: Int -> Bool -> Amount -> Amount -> MyWriterT (Amount, Int) (MyStateT (StdGen, Amount) (Either String)) Amount
simulate nbRounds mustHitSoftSeventeen refillAmount stake = foldr f (return refillAmount) [1..nbRounds]
                                                            where f _ acc = acc >>= (\newBalance -> if newBalance < stake 
                                                                                                      then refillBalance refillAmount 
                                                                                                      else play1round mustHitSoftSeventeen stake)

refillBalance :: Amount -> MyWriterT (Amount, Int) (MyStateT (StdGen, Amount) (Either String)) Amount 
refillBalance refillAmount = MyWriterT (MyStateT (\(gen, balance) -> ((refillAmount, mempty), (gen, refillAmount)))

play1round :: Bool -> Amount -> MyWriterT (Amount, Int) (MyStateT (StdGen, Amount) (Either String)) Amount
play1round mustHitSoftSeventeen stake = MyWriterT (MyStateT f)
  where f (gen, initialBalance) = if (initialBalance - stake < 0) 
                                    then Left "Not enough money left to play this round!"
                                    else let (bankHand, gen1) = drawHand gen
                                             (playerHand, gen2) = drawHand gen1
                                             upCard = head bankHand
                                             playerInitialStatus = Start (Bet (playerHand, stake))
                                         in if (isBlackJack bankHand)
                                              then let gains = -stake
                                                       newBalance = initialBalance + gains                                                        
                                                   in Right ((newBalance, (gains, 1)), (gen2, newBalance))
                                              else (runMyStateT (playBasicStrategy upCard playerInitialStatus) $ (gen2, initialBalance, 0)) >>= ( 
                                                \(statusList, (gen3, updatedBalance, _)) -> let (bankScore, gen4) = runState (playBank mustHitSoftSeventeen bankHand) $ gen3
                                                                                                gains = totalGains bankScore statusList 
                                                                                            in gains >>= (\g -> let newBalance = updatedBalance + g 
                                                                                                                in Right ((newBalance, (g, 1)), (gen4, newBalance))))
                                                                                      
playBank :: Bool -> Hand -> State StdGen Int
playBank mustHitSoftSeventeen h = if (bestScore h < 16 || mustHitSoftSeventeen && bestScore h == 17 && isSoftHand h)
                                    then state (\gen -> let (c, gen') = drawCard gen in (c:h, gen')) >>= playBank mustHitSoftSeventeen
                                    else return (bestScore h)


totalGains :: Int -> [Status] -> Either String Amount
totalGains bankScore = foldr f (Right 0)
                       where f s@(Start _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(StartSplit _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(StartNoSplit _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(Doubled _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(Continue _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f (Bust _) acc = acc
                             f (BlackJack a) acc = acc >>= (\totalGains -> return (totalGains + (1 + blackJackQuote) * a))
                             f (Surrender a) acc = acc >>= (\totalGains -> return (totalGains + a / 2))
                             f (Stand (Bet (h, a))) acc = acc >>= (\totalGains -> if (bestScore h > bankScore || bankScore > 21) 
                                                                                      then return (totalGains + 2 * a)
                                                                                      else acc)
