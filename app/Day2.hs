import Data.List.Split
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- readFile "inputs/Day2.txt"
    let s1 = sumInvalidIds <$> parse input
    putStrLn $ "Part 1: " ++ show s1

sumInvalidIds :: [[Integer]] -> Integer
sumInvalidIds = sum . map (sum . filter (not . valid))

data Range = Range { from :: Integer, to :: Integer } deriving Show

valid :: Integer -> Bool
valid num = case even numLen of
    True ->
        let halves = splitAt (div numLen 2) numAsStr
        in fst halves /= snd halves
    False -> True
    where
    numAsStr = show num
    numLen = length numAsStr

parse :: String -> Maybe [[Integer]]
parse input = traverse parseRange $ splitOn "," input

parseRange :: String -> Maybe [Integer]
parseRange input = case splitOn "-" input of
    [a, b] -> do
        from <- readMaybe a
        to <- readMaybe b
        if from > to then Nothing
        else pure $ [from..to]
    otherwise -> Nothing
