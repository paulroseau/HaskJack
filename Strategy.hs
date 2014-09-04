module Strategy (playBasicStrategy) where

import Cards
import Commons
import Data.Array
import Game
import System.Random (StdGen)

playBasicStrategy :: Card -> Status -> MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status] 
playBasicStrategy _ s@(Bust _) = return [s]
playBasicStrategy _ s@(BlackJack _) = return [s]
playBasicStrategy _ s@(Surrender _) = return [s]
playBasicStrategy _ s@(Stand _) = return [s]
playBasicStrategy upCard s@(Doubled (Bet (h, _))) = regularLookup h upCard s >>= playBasicStrategy' upCard
playBasicStrategy upCard s@(Continue (Bet (h, _))) = regularLookup h upCard s >>= playBasicStrategy' upCard
playBasicStrategy upCard s@(StartNoSplit (Bet (h, _))) = regularLookup h upCard s >>= playBasicStrategy' upCard
playBasicStrategy upCard s@(StartSplit (Bet (h, _))) = startLookup h upCard s >>= playBasicStrategy' upCard
playBasicStrategy upCard s@(Start (Bet (h, a))) = if isBlackJack h
                                                     then return [BlackJack a]
                                                     else startLookup h upCard s >>= playBasicStrategy' upCard

playBasicStrategy' :: Card -> [Status] -> MyStateT (StdGen, Amount, NumberOfSplits) (Either String) [Status]
playBasicStrategy' upCard xs = foldr f (return []) xs
                               where f x acc = playBasicStrategy upCard x >>= (\xList -> acc >>= (\accList -> return (xList ++ accList)))
                                                                                      
regularLookup :: Hand -> Card -> Move
regularLookup h c = (if isSoftHand h 
                       then softHandBasicStrategy 
                       else hardHandBasicStrategy) ! (bestScore h, bestScore [c])

-- TODO think about the return type because head is unsafe
startLookup :: Hand -> Card -> Move
startLookup h c = if isPair h 
                    then splitHandBasicStrategy ! (bestScore [head h], bestScore [c])
                    else regularLookup h c 

-- Strategy definition
type Strategy = Array (Int, Card) Move

hardHandBasicStrategy  :: Array (Int, Int) Move
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

-- TODO add a row for double aces (12)
softHandBasicStrategy :: Array (Int, Int) Move
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

splitHandBasicStrategy :: Array (Int, Int) Move
splitHandBasicStrategy = array ((2, 2), (11, 11)) [
                          ((2, 2), p), ((2, 3), p), ((2, 4), p), ((2, 5), p), ((2, 6), p), ((2, 7), p), ((2, 8), h), ((2, 9), h), ((2, 10), h), ((2, 11), h), 
                          ((3, 2), p), ((3, 3), p), ((3, 4), p), ((3, 5), p), ((3, 6), p), ((3, 7), p), ((3, 8), h), ((3, 9), h), ((3, 10), h), ((3, 11), h), 
                          ((4, 2), h), ((4, 3), h), ((4, 4), h), ((4, 5), p), ((4, 6), p), ((4, 7), h), ((4, 8), h), ((4, 9), h), ((4, 10), h), ((4, 11), h), 
                          ((5, 2), dh), ((5, 3), dh), ((5, 4), dh), ((5, 5), dh), ((5, 6), dh), ((5, 7), dh), ((5, 8), dh), ((5, 9), dh), ((5, 10), h), ((5, 11), h), 
                          ((6, 2), p), ((6, 3), p), ((6, 4), p), ((6, 5), p), ((6, 6), p), ((6, 7), h), ((6, 8), h), ((6, 9), h), ((6, 10), h), ((6, 11), h), 
                          ((7, 2), p), ((7, 3), p), ((7, 4), p), ((7, 5), p), ((7, 6), p), ((7, 7), p), ((7, 8), h), ((7, 9), h), ((7, 10), h), ((7, 11), h), 
                          ((8, 2), p), ((8, 3), p), ((8, 4), p), ((8, 5), p), ((8, 6), p), ((8, 7), p), ((8, 8), p), ((8, 9), p), ((8, 10), p), ((8, 11), rh), 
                          ((9, 2), p), ((9, 3), p), ((9, 4), p), ((9, 5), p), ((9, 6), p), ((9, 7), p), ((9, 8), s), ((9, 9), p), ((9, 10), s), ((9, 11), s), 
                          ((10, 2), s), ((10, 3), s), ((10, 4), s), ((10, 5), s), ((10, 6), s), ((10, 7), s), ((10, 8), s), ((10, 9), s), ((10, 10), s), ((10, 11), s), 
                          ((11, 2), p), ((11, 3), p), ((11, 4), p), ((11, 5), p), ((11, 6), p), ((11, 7), p), ((11, 8), p), ((11, 9), p), ((11, 10), p), ((11, 11), p)] 

-- Alias
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
