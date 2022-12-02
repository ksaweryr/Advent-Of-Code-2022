import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.Bifunctor (second)

class Score a where
  toScore :: a -> Int

data Result = Won | Lost | Draw

instance Score Result where
  toScore Won = 6
  toScore Draw = 3
  toScore Lost = 0

instance Score Char where
  toScore 'X' = 1
  toScore 'Y' = 2
  toScore 'Z' = 3
  toScore _ = error "Shouldn't happen"

getResult :: Char -> Char -> Result
getResult 'A' 'Y' = Won
getResult 'A' 'Z' = Lost
getResult 'B' 'X' = Lost
getResult 'B' 'Z' = Won
getResult 'C' 'X' = Won
getResult 'C' 'Y' = Lost
getResult _ _ = Draw

getShape :: Char -> Result -> Char
getShape 'A' Won = 'Y'
getShape 'A' Lost = 'Z'
getShape 'A' Draw = 'X'
getShape 'B' Won = 'Z'
getShape 'B' Lost = 'X'
getShape 'B' Draw = 'Y'
getShape 'C' Won = 'X'
getShape 'C' Lost = 'Y'
getShape 'C' Draw = 'Z'
getShape _ _ = error "Shouldn't happen"

decodeResult :: Char -> Result
decodeResult 'X' = Lost
decodeResult 'Y' = Draw
decodeResult 'Z' = Won
decodeResult _ = error "Shouldn't happen"

scoreRound :: Char -> Char -> Int
scoreRound a = liftM2 (+) toScore (toScore . getResult a)

parseInput :: String -> [(Char, Char)]
parseInput = map (liftM2 (,) head last . map head . words) . lines

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ sum $ uncurry scoreRound <$> input
  print $ sum $ uncurry (+) <$> (toScore . snd &&& toScore . uncurry getShape) <$> second decodeResult <$> input
