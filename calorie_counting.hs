{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow((>>>))
import Data.List (sortBy)
import Data.Ord
import qualified Data.Text as T

solve :: Int -> [[Int]] -> Int
solve cnt input = (sum . take cnt . sortBy (comparing Down) . map sum) input

parseInput :: String -> [[Int]]
parseInput = T.pack >>> T.splitOn "\n\n" >>> map (map (read . T.unpack) . T.lines)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ solve 1 input
  print $ solve 3 input
