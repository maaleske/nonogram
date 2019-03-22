module Examples where
import Data.Maybe
import Hints
import Picture

data Puzzle = Puzzle {
  size :: (Int, Int)
 ,lineHints :: [Hints]
 ,rankHints :: [Hints]
 } deriving (Show, Eq)

solve :: Puzzle -> Maybe Picture
solve p = let (w, h) = size p
    in listToMaybe . map (Picture h w) $ solvePossible w h (lineHints p) (rankHints p)

letterY = Puzzle (3, 3) [[1, 1], [1], [1]] [[1], [2], [1]]

heart = Puzzle (5, 5) [[1, 1], [1, 1, 1], [1, 1], [1, 1], [1]] [[2], [1, 1], [1, 1], [1, 1], [2]]

square = Puzzle (6, 6) [[6], [1,1], [1,1], [1,1], [1,1], [6]] [[6], [1,1], [1,1], [1,1], [1,1], [6]]

bunny = Puzzle (8, 8) [[0], [1], [1], [2, 2], [1, 5], [8], [6], [2, 3]] [[2], [1, 1, 1], [7], [3], [5], [5], [4], [2]]

apple = Puzzle (10, 10) [[1], [3, 1, 2], [4, 3], [2, 7], [10], [2, 7], [10], [8], [6]] [[6], [8], [2, 1, 3], [9], [7], [2, 7], [7], [9], [8], [6]]
