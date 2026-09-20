import Data.List.Split
import Text.Read (readMaybe)
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Control.Monad.State

main :: IO ()
main = do
    banks <- parse <$> readFile "inputs/Day3.txt"
    let s1 = maxJoltageSum  2 <$> banks
    let s2 = maxJoltageSum 12 <$> banks
    putStrLn $ "Part 1: " ++ show s1
    putStrLn $ "Part 2: " ++ show s2

maxJoltageSum :: Int -> [[Integer]] -> Integer
maxJoltageSum digits banks = sum $ map (maxJoltage (digits)) banks

-- Greedy algorithm - begin from the left most available battery,
-- but also leave enough batteries on the right and choose one with
-- the largest joltage, do that recursively.
maxJoltage :: Int -> [Integer] -> Integer
maxJoltage digits bank = evalState (go digits) bank where
    go :: Int -> State [Integer] Integer
    go 1 = chooseBattery 0
    go digit = do
        b <- chooseBattery (digit - 1)
        rem <- go (digit - 1)
        pure $ b * 10 ^ (digit - 1) + rem

chooseBattery :: Int -> State [Integer] Integer
chooseBattery remaining = do
    bank <- get
    let available = take (length bank - remaining) bank
    let (skipped, battery) = maxWithIdx available
    put $ drop (skipped + 1) bank
    pure battery

-- It is very important that this method returns the index
-- of the first occurance of the largest element.
maxWithIdx :: Ord a => [a] -> (Int, a)
maxWithIdx xs = (i, a) where
    a = maximum xs
    i = fromJust $ elemIndex a xs

parse :: String -> Maybe [[Integer]]
parse = traverse parseBank . lines

parseBank :: String -> Maybe [Integer]
parseBank = traverse readMaybe . map pure
