import Examples
import Data.Maybe

main :: IO ()
main = do
    putStrLn . show . fromJust $ solve letterY
    putStrLn . show . fromJust $ solve heart
--    putStrLn . show . fromJust $ solve bunny
