module Picture where
import Hints

-- Picture size and list of booleans for each pixel in the image marking the state
data Picture = Picture {
   height :: Int
  ,width :: Int
  ,pixels :: [Square]
  } deriving (Eq)

instance Show (Picture) where
    show = showPic

showPic :: Picture -> String
showPic (Picture 1 _ px) = concatMap show px
showPic pic@(Picture h w px) = unlines . map showPic $ map (row pic) [1..h]

emptyPicture :: Int -> Int -> Picture
emptyPicture h w = Picture h w . take (h * w) $ repeat Blank

fullPicture :: Int -> Int -> Picture
fullPicture h w = Picture h w . take (h * w) $ repeat Filled

readPicture :: String -> Picture
readPicture s = Picture h w px
    where px = map readPx . concat $ rows
          rows = lines s
          w = length . head $ rows
          h = length rows
          readPx '█' = Filled
          readPx '·' = Blank

rowsUntil :: Picture -> Int -> Picture
rowsUntil (Picture h w px) n = Picture (h - n + 1) w . take (n * w) $ px

row :: Picture -> Int -> Picture
row pic@(Picture h w px) n = Picture 1 w . drop ((n - 1) * w) . pixels $ rowsUntil pic n
