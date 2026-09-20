import Data.List.Split
import Text.Read
import Data.List
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "inputs/Day3.txt"
    let s1 = sum . map maxJoltage <$> parse input
    putStrLn $ "Part 1: " ++ show s1

maxJoltage :: [Int] -> Int
maxJoltage b = 10 * digit1 + digit2 where
    digit1 :: Int
    digit1 = foldr max 0 $ init b
    digit1Id = fromJust $ elemIndex digit1 b
    digit2 = foldr max 0 $ drop (digit1Id + 1) b

parse :: String -> Maybe [[Int]]
parse = traverse parseBank . lines

parseBank :: String -> Maybe [Int]
parseBank = traverse readMaybe . map pure
