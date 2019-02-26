module Hints where

import Data.List

-- Square status
data Square = Filled | Blank deriving (Eq)

-- List of hint integers
type Hints = [Int]
type Emptys = [Int]


solvePossible :: Int -> Int -> [Hints] -> [Hints] -> [[Square]]
solvePossible w h hhints vhints = intersect imsh imsv
    where imsh = map concat $ possibleImages w hhints
          imsv = map (concat . transpose) $ possibleImages h vhints

    
possibleImages :: Int -> [Hints] -> [[[Square]]]
possibleImages _ [] = [[]]
possibleImages l (h:hs) = do
    line <- possibleLines l h
    rest <- possibleImages l hs
    pure $ line : rest

possibleLines :: Int -> Hints -> [[Square]]
possibleLines l hints =
    let allowedEmpty = (1 >=) . abs . (length hints -) . length in
    do
        emptys <- filter allowedEmpty $ divideToNats (l - sum hints)
        lines <- possibleLinesL hints emptys ++ possibleLinesR hints emptys
        pure lines

possibleLinesL :: Hints -> Emptys -> [[Square]]
possibleLinesL [] [] = [[]]
possibleLinesL [h] [] = [nFilled h]
possibleLinesL [] [e] = []
possibleLinesL (h:hs) (e:es) = do 
    rest <- possibleLinesL hs es
    pure $ nFilled h ++ nBlank e ++ rest

possibleLinesR :: Hints -> Emptys -> [[Square]]
possibleLinesR [] [] = [[]]
possibleLinesR [h] [] = []
possibleLinesR [] [e] = [nBlank e]
possibleLinesR (h:hs) (e:es) = do 
    rest <- possibleLinesR hs es
    pure $ nBlank e ++ nFilled h ++ rest

nBlank :: Int -> [Square]
nBlank = flip replicate Blank

nFilled :: Int -> [Square]
nFilled = flip replicate Filled

divideToNats :: Int -> [[Int]]
divideToNats 0 = [[]]
divideToNats n = do
    k <- [n,n-1..1]
    rest <- divideToNats (n - k)
    pure $ k : rest
