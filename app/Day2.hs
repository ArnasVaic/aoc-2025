import Data.List.Split
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- readFile "inputs/Day2.txt"
    let idRanges = parse input
    let s1 = sumIds (not . isIdValidP1) <$> idRanges
    putStrLn $ "Part 1: " ++ show s1

    let s2 = sumIds (not . isIdValidP2) <$> idRanges
    putStrLn $ "Part 2: " ++ show s2

sumIds :: (Integer -> Bool) -> [[Integer]] -> Integer
sumIds f = sum . map (sum . filter f)

isIdValidP1 :: Integer -> Bool
isIdValidP1 num = case even numLen of
    True ->
        let halves = splitAt (div numLen 2) numAsStr
        in fst halves /= snd halves
    False -> True
    where
    numAsStr = show num
    numLen = length numAsStr

isIdValidP2 :: Integer -> Bool
isIdValidP2 = not . isRepeating . show

properDivisors :: Integral a => a -> [a]
properDivisors n = [d | d <- [1..n-1], mod n d == 0]

isRepeating :: String -> Bool
isRepeating s = any id $ map (\n -> isRepeatingN n s) divs where
    divs = properDivisors $ length s

isRepeatingN :: Int -> String -> Bool
isRepeatingN n s = all (== chunk) $ chunksOf n s
    where chunk = take n s

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
