import Data.List (find)
import Data.Maybe (fromJust)

window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = take n xs : (window n $ tail xs)

allDistinct :: Eq a => [a] -> Bool
allDistinct [] = True
allDistinct (x:xs) = not (x `elem` xs) && allDistinct xs

solve :: Int -> String -> Int
solve n = (+) n . fst . fromJust . find (allDistinct . snd) . zip [0..] . window n

main :: IO ()
main = do
    input <- getLine
    print $ solve 4 input
    print $ solve 14 input