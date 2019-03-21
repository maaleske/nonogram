import Examples
import Data.Maybe

import Data.Time.Clock


main :: IO ()
main = do
    t0 <- getCurrentTime
    putStrLn . show . fromJust $ solve letterY
    t1 <- getCurrentTime
    putStrLn $ "Solution took " ++ show (diffUTCTime t1 t0)
    putStrLn . show . fromJust $ solve heart
    t2 <- getCurrentTime
    putStrLn $ "Solution took " ++ show (diffUTCTime t2 t1)
    putStrLn . show . fromJust $ solve square
    t3 <- getCurrentTime
    putStrLn $ "Solution took " ++ show (diffUTCTime t3 t2)
--    putStrLn . show . fromJust $ solve bunny
