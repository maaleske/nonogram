module Hints.Types where

-- Square status
data Square = Filled | Blank deriving (Eq, Ord)

instance Show Square where
    show Filled = "█"
    show Blank = "·"

-- List of hint integers
type Hints = [Int]
type Emptys = [Int]
