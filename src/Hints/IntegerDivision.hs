module Hints.IntegerDivision where

import Data.List
import Hints.Types

solvePossible :: Int -> Int -> [Hints] -> [Hints] -> [[Square]]
solvePossible w h hhints vhints = intersect imsh imsv
    where imsh = map concat $ possibleImages w hhints
          imsv = map (concat . transpose) $ possibleImages h vhints

possibleImages :: Int -> [Hints] -> [[[Square]]]
possibleImages _ [] = [[]]
possibleImages l (h:hs) = do
    line <- possibleLines' l h
    rest <- possibleImages l hs
    pure $ line : rest

possibleLines :: Int -> Hints -> [[Square]]
possibleLines l hints =
    let allowedEmpty = (1 >=) . abs . (length hints -) . length in
    do
        emptys <- filter allowedEmpty $ divideToNats (l - sum hints)
        lines <- possibleLinesL hints emptys ++ possibleLinesR hints emptys
        pure lines

possibleLines' :: Int -> Hints -> [[Square]]
possibleLines' l hints = let
    nemptys = let nh = length hints in [nh - 1, nh, nh + 1]
    possibleEmptys = concatMap (flip divideToKNats (l - sum hints)) nemptys
    in do
        emptys <- possibleEmptys
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

memo2 :: (Int -> Int -> a) -> [[a]]
memo2 f = map (\x -> map (f x) [0..]) [0..]

divideToKNats :: Int -> Int -> [[Int]]
divideToKNats k n = knDivisions !! k !! n

knDivisions :: [[[[Int]]]]
knDivisions = memo2 divideToKNats'

divideToKNats' :: Int -> Int -> [[Int]]
divideToKNats' 0 0 = [[]]
divideToKNats' 0 _ = []
divideToKNats' _ 0 = []
divideToKNats' 1 n = [[n]]
divideToKNats' k n = do
    m <- [1..n]
    rest <- divideToKNats (k-1) (n-m)
    pure $ m : rest

divideToNats :: Int -> [[Int]]
divideToNats = (map divideToNats' [0..] !!)

divideToNats' :: Int -> [[Int]]
divideToNats' 0 = [[]]
divideToNats' n = do
    k <- [1..n]
    rest <- divideToNats (n - k)
    pure $ k : rest
