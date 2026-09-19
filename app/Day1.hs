{-# LANGUAGE ScopedTypeVariables #-}

import Text.Read (readMaybe)
import Debug.Trace
import Data.Maybe

main :: IO ()
main = do
    s1 <- getSolution solve1 "inputs/Day1.txt"
    putStrLn $ "Part 1: " ++ show s1

    s2 <- getSolution solve2 "inputs/Day1.txt"
    putStrLn $ "Part 2: " ++ show s2

solve1 :: [Rotation] -> Int
solve1 rs = countPred rs (Rotation 50) sumToZero

sumToZero :: Rotation -> Rotation -> Int
sumToZero a b = fromEnum $ (a <> b) == mempty

solve2 :: [Rotation] -> Int
solve2 rs = countPred rs (Rotation 50) rotations

rotations :: Rotation -> Rotation -> Int
rotations (Rotation a) (Rotation b)
    -- For all left rotations from 0, q is always
    -- off by 1 because going left from 0 by less
    -- than 100 steps doesn't count as going through 0
    | a == 0 && b < 0 = -1 + abs q

    -- For all left rotations from non 0 start
    -- that end on 0 q will be off by 1.
    -- Example: div (20 - 120) 100 = -1
    -- in truth this rotation would got trough 0
    -- once and end up on 0 again at the end so
    -- result is 2
    | a > 0 && b < 0 && r == 0 = 1 + abs q

    -- Base case - division works well
    | otherwise = abs q

    where (q, r) = divMod (a + b) 100

-- countPred is a terrible name but I am too lazy to come up with a good
-- method name for this function.
--
-- Sum up the return values of given function f which returns a value based on
-- the cummulative concatenation of elements and the next element in line.
countPred :: forall m. Monoid m => [m] -> m -> (m -> m -> Int) -> Int
countPred xs x0 f = snd $ foldl' step (x0, 0) xs where
    step :: (m, Int) -> m -> (m, Int)
    step (acc, cnt) el = (acc <> el, cnt + f acc el)

newtype Rotation = Rotation Int deriving (Show, Eq)

instance Semigroup Rotation where
    Rotation a <> Rotation b = Rotation $ mod (a + b) 100

instance Monoid Rotation where
    mempty = Rotation 0

parse :: String -> Maybe Rotation
parse ('L':xs) = Rotation . negate <$> readMaybe xs
parse ('R':xs) = Rotation <$> readMaybe xs
parse _ = Nothing

symMod :: Integral a => a -> a -> a
symMod a m = let half_m = div m 2 in
    mod (a + half_m) m - half_m

getSolution :: ([Rotation] -> Int) -> String -> IO Int
getSolution solver filename = do
    file <- readFile filename
    let rs = traverse parse $ lines file
    -- Nothing never really happens because traverse parse [] is Just []
    return $ fromMaybe (-1) (solver <$> rs)
