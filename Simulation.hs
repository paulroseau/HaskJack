module Simulation where

import Cards
import Commons
import Control.Monad.State
import Game
import Strategy
import System.Random (StdGen)
import Data.Monoid (mempty)

getGainsEstimate :: Int -> Bool -> Amount -> Amount -> StdGen -> Either String Amount
getGainsEstimate nbRounds mustHitSoftSeventeen refillAmount chipValue seed = let simulation = simulate nbRounds mustHitSoftSeventeen refillAmount chipValue
                                                                                 cumulatedGains = (runMyStateT . runMyWriterT $ simulation) $ (seed, refillAmount)
                                                                             in cumulatedGains >>= (\(((Amount c), _), (_, _)) -> return (Amount (c / (fromIntegral nbRounds))))

simulate :: Int -> Bool -> Amount -> Amount -> MyWriterT Amount (MyStateT (StdGen, Amount) (Either String)) Amount
simulate nbRounds mustHitSoftSeventeen refillAmount chipValue = foldr f (return refillAmount) [1..nbRounds]
                                                            where f _ acc = acc >>= (\newBalance -> if newBalance < chipValue 
                                                                                                      then refillBalance refillAmount 
                                                                                                      else play1round mustHitSoftSeventeen chipValue)

refillBalance :: Amount -> MyWriterT Amount (MyStateT (StdGen, Amount) (Either String)) Amount 
refillBalance refillAmount = MyWriterT $ MyStateT (\(gen, balance) -> return ((refillAmount, mempty), (gen, refillAmount)))

play1round :: Bool -> Amount -> MyWriterT Amount (MyStateT (StdGen, Amount) (Either String)) Amount
play1round mustHitSoftSeventeen chipValue = MyWriterT (MyStateT f)
  where f (gen, initialBalance) = if (initialBalance - chipValue < 0) 
                                    then Left "Not enough money left to play this round!"
                                    else let (bankHand, gen1) = drawHand gen
                                             (playerHand, gen2) = drawHand gen1
                                             upCard = head bankHand
                                             playerInitialStatus = Start (Bet (playerHand, chipValue))
                                         in if (isBlackJack bankHand)
                                              then let gains = -chipValue
                                                       newBalance = initialBalance + gains                                                        
                                                   in Right ((newBalance, gains), (gen2, newBalance))
                                              else (runMyStateT (playBasicStrategy upCard playerInitialStatus) $ (gen2, initialBalance, 0)) >>= ( 
                                                \(statusList, (gen3, updatedBalance, _)) -> let (bankScore, gen4) = runState (playBank mustHitSoftSeventeen bankHand) $ gen3
                                                                                                gains = totalGains bankScore statusList 
                                                                                            in gains >>= (\g -> let newBalance = updatedBalance + g 
                                                                                                                in Right ((newBalance, g), (gen4, newBalance))))
                                                                                      
playBank :: Bool -> Hand -> State StdGen Int
playBank mustHitSoftSeventeen h = if (bestScore h < 16 || mustHitSoftSeventeen && bestScore h == 17 && isSoftHand h)
                                    then state (\gen -> let (c, gen') = drawCard gen in (c:h, gen')) >>= playBank mustHitSoftSeventeen
                                    else return (bestScore h)


totalGains :: Int -> [Status] -> Either String Amount
totalGains bankScore = foldr f (Right (Amount 0))
                       where f s@(Start _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(StartSplit _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(StartNoSplit _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(Doubled _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f s@(Continue _) acc = Left ("Cannot compute gains on non-final status : " ++ show s ++ ".")
                             f (Bust a) acc = acc >>= (\totalGains -> return (totalGains - a))
                             f (BlackJack (Amount a)) acc = acc >>= (\totalGains -> return (totalGains + Amount (blackJackQuote * a)))
                             f (Surrender (Amount a)) acc = acc >>= (\totalGains -> return (totalGains - Amount (a / 2)))
                             f (Stand (Bet (h, a))) acc = acc >>= (\totalGains -> if (bestScore h > bankScore || bankScore > 21) 
                                                                                      then return (totalGains + a)
                                                                                      else return (totalGains - a))
