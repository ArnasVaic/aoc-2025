import qualified Data.Vector as V
import Text.Parsec.String (Parser)
import Text.Parsec
import Control.Applicative (some)
import Data.Either
import Data.List.Split hiding (sepBy)
import Text.Parsec.Token
import Control.Monad
import Data.List

data Grid a = Grid {
    width :: Int,
    height :: Int,
    values :: V.Vector a
}

instance Show a => Show (Grid a) where
    show (Grid w h vals) = concat $ intersperse "\n" $ chunksOf w $ concat $ map show $ V.toList vals

data Cell = PaperRoll | EmptyCell deriving Eq

instance Show Cell where
    show PaperRoll = "@"
    show EmptyCell = "."

data Coord = Coord { row :: Int, col :: Int } deriving (Eq, Show)

instance Semigroup Coord where
    (Coord r1 c1) <> (Coord r2 c2) = Coord (r1 + r2) (c1 + c2)

main :: IO ()
main = do
    input <- readFile "inputs/Day4.txt"
    let gridResult = parse gridP "" input
    let s1 = accessiblePaperRollCount <$> gridResult
    putStrLn $ "Part 1: " ++ show s1

isAccessible :: Grid Cell -> Coord -> Bool
isAccessible grid coord = 4 > neighbourCount grid coord

accessiblePaperRollCount :: Grid Cell -> Int
accessiblePaperRollCount grid@(Grid w h cells) =
    let rollCoords = paperRollCoordinates grid
        canAccess = isAccessible grid
    in length $ filter canAccess rollCoords

paperRollCoordinates :: Grid Cell -> [Coord]
paperRollCoordinates grid@(Grid w h _) = do
    row <- [0.. h - 1]
    col <- [0 .. w - 1]
    let coord = Coord row col
    guard $ grid ! coord == PaperRoll
    pure coord

relativeNeighbourCoords :: [Coord]
relativeNeighbourCoords = do
    row <- [-1..1]
    col <- [-1..1]
    guard (row /= 0 || col /= 0)
    pure $ Coord row col

neighbours :: Coord -> [Coord]
neighbours p = map (p <>) relativeNeighbourCoords

neighbourCount :: Grid Cell -> Coord -> Int
neighbourCount grid p =
    let neighs = map (grid !) $ filter (contains grid) (neighbours p)
    in length $ filter (== PaperRoll) neighs

contains :: Grid a -> Coord -> Bool
contains (Grid w h _) (Coord row col) = withinW && withinH
    where
        withinH = row >= 0 && row < h
        withinW = col >= 0 && col < w

(!) :: Grid a -> Coord -> a
(Grid w _ vals) ! (Coord row col) = vals V.! (row * w + col)

gridP :: Parser (Grid Cell)
gridP = do
    rows <- (some cell) `sepEndBy` newline
    let (w, h) = ((length . head) rows, length rows)
    let total = (sum . map length) rows
    let vector = (V.concat . map V.fromList) rows
    if V.length vector /= w * h
        then fail "parsing failed, input grid is jagged"
        else pure $ Grid w h $ vector

cell :: Parser Cell
cell = (char '@' >> pure PaperRoll) <|> (char '.' >> pure EmptyCell)
