import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Control.Monad.State

newtype Battery = Battery { joltageRating :: Integer } deriving Eq
newtype Bank = Bank { batteries :: [Battery] }

main :: IO ()
main = do
    banks <- parse <$> readFile "inputs/Day3.txt"
    let joltSum n = sum . map (maxJoltage n)

    let s1 = joltSum  2 <$> banks
    putStrLn $ "Part 1: " ++ show s1

    let s2 = joltSum 12 <$> banks
    putStrLn $ "Part 2: " ++ show s2

-- Given a number of batteries to be chosen and a bank
-- determine the maximal possible joltage.
--
-- Greedy algorithm implementation - begin from the left most available battery,
-- but also leave enough batteries on the right and choose one with
-- the largest joltage, do that recursively.
maxJoltage :: Int -> Bank -> Integer
maxJoltage n bank = evalState (go (n - 1)) bank where
    go :: Int -> State Bank Integer
    go 0 = joltageRating <$> chooseBattery 0
    go remaining = do
        battery <- chooseBattery remaining
        let rating = joltageRating battery

        -- Joltage value of the remaining batteries
        rest <- go (remaining - 1)

        -- Battery joltage ratings are digits of the joltage value
        pure $ rating * 10 ^ remaining + rest

-- Choosing a battery shrinks the bank such that
-- only batteries after it could be chosen. Choosing
-- is also affected by the remaining number of batteries
-- to be chosen.
chooseBattery :: Int -> State Bank Battery
chooseBattery remaining = do
    bank <- get
    let available = reserve remaining bank
    let battery = maximalBattery available
    -- Chosen battery is always within the bank
    let bank' = fromMaybe undefined $ shrink bank battery
    put bank'
    pure battery

-- Return a subset of the bank without the last `remaining` batteries
reserve :: Int -> Bank -> Bank
reserve remaining (Bank bs) = Bank $ take n bs
    where n = length bs - remaining

maximalBattery :: Bank -> Battery
maximalBattery (Bank batteries) = Battery $ maximum $ map joltageRating batteries

shrink :: Bank -> Battery -> Maybe Bank
shrink (Bank batteries) chosen = do
    index <- elemIndex chosen batteries
    -- Exclude not only batteries to the left
    -- but also the chosen battery
    pure $ Bank $ drop (index + 1) batteries

parse :: String -> Maybe [Bank]
parse = traverse parseBank . lines

parseBank :: String -> Maybe Bank
parseBank input = Bank <$> (traverse parseBattery . map pure) input

parseBattery :: String -> Maybe Battery
parseBattery = (fmap Battery) . readMaybe
