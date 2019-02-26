module Hints where

import Data.List

-- List of hint integers
type Hints = [Int]
type Emptys = [Int]


solvePossible :: Int -> Int -> [Hints] -> [Hints] -> [[Bool]]
solvePossible w h hhints vhints = intersect imsh imsv
    where imsh = map concat $ possibleImages w hhints
          imsv = map (concat . transpose) $ possibleImages h vhints

    
possibleImages :: Int -> [Hints] -> [[[Bool]]]
possibleImages _ [] = [[]]
possibleImages l (h:hs) = do
    line <- possibleLines l h
    rest <- possibleImages l hs
    pure $ line : rest

possibleLines :: Int -> Hints -> [[Bool]]
possibleLines l hints =
    let allowedEmpty = (1 >=) . abs . (length hints -) . length in
    do
        emptys <- filter allowedEmpty $ divideToNats (l - sum hints)
        lines <- possibleLinesL hints emptys ++ possibleLinesR hints emptys
        pure lines

possibleLinesL :: Hints -> Emptys -> [[Bool]]
possibleLinesL [] [] = [[]]
possibleLinesL [h] [] = [nTrue h]
possibleLinesL [] [e] = []
possibleLinesL (h:hs) (e:es) = do 
    rest <- possibleLinesL hs es
    pure $ nTrue h ++ nFalse e ++ rest

possibleLinesR :: Hints -> Emptys -> [[Bool]]
possibleLinesR [] [] = [[]]
possibleLinesR [h] [] = []
possibleLinesR [] [e] = [nFalse e]
possibleLinesR (h:hs) (e:es) = do 
    rest <- possibleLinesR hs es
    pure $ nFalse e ++ nTrue h ++ rest

nFalse :: Int -> [Bool]
nFalse = flip replicate False

nTrue :: Int -> [Bool]
nTrue = flip replicate True

divideToNats :: Int -> [[Int]]
divideToNats 0 = [[]]
divideToNats n = do
    k <- [n,n-1..1]
    rest <- divideToNats (n - k)
    pure $ k : rest
