module Picture where

-- Picture size and list of booleans for each pixel in the image marking the state
data Picture = Picture {
   height :: Int
  ,width :: Int
  ,pixels :: [Bool]
  } deriving (Eq)

instance Show (Picture) where
    show = showPic

showPic :: Picture -> String
showPic (Picture 1 _ px) = map showPx px
  where showPx True = '█'
        showPx False = '·'
showPic pic@(Picture h w px) = unlines . map showPic $ map (row pic) [1..h]

emptyPicture :: Int -> Int -> Picture
emptyPicture h w = Picture h w . take (h * w) $ repeat False

fullPicture :: Int -> Int -> Picture
fullPicture h w = Picture h w . take (h * w) $ repeat True

readPicture :: String -> Picture
readPicture s = Picture h w px
    where px = map (=='█') . concat $ rows
          rows = lines s
          w = length . head $ rows
          h = length rows

rowsUntil :: Picture -> Int -> Picture
rowsUntil (Picture h w px) n = Picture (h - n + 1) w . take (n * w) $ px

row :: Picture -> Int -> Picture
row pic@(Picture h w px) n = Picture 1 w . drop ((n - 1) * w) . pixels $ rowsUntil pic n
