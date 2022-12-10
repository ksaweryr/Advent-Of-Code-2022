{-# Language ScopedTypeVariables #-}
import Data.Bool (bool)
import Data.Char (toUpper)

data Instruction = Addx Int | Noop deriving(Read)
data CpuState = CpuState { x :: Int, time :: Int } deriving(Show)

runInstruction :: CpuState -> Instruction -> CpuState
runInstruction s@(CpuState _ time) Noop = s { time = time + 1 }
runInstruction (CpuState x time) (Addx v) = CpuState (x + v) (time + 2)

runProgram :: [Instruction] -> [CpuState]
runProgram p = runProgram' (CpuState 1 0) p
    where
        runProgram' :: CpuState -> [Instruction] -> [CpuState]
        runProgram' s [] = [s]
        runProgram' s (p:ps) = s : runProgram' (runInstruction s p) ps

task1 :: [CpuState] -> Int
task1 s = task1' s 20
    where
        task1' :: [CpuState] -> Int -> Int
        task1' _ 260 = 0
        task1' s maxTime = getScore (last before) + task1' after (maxTime + 40)
            where
                (before, after) = span ((< maxTime) . time) s
                getScore :: CpuState -> Int
                getScore (CpuState x _) = x * maxTime

-- not prefect but good enough to read the letters
task2 :: [CpuState] -> [String]
task2 s = [line s x | x <- [0..5]]
    where
        line :: [CpuState] -> Int -> String
        line s n = bool '.' '#' <$> line' (dropWhile ((< n') . time) s) n'
            where
                n' = n * 40
                line' :: [CpuState] -> Int -> [Bool]
                line' (c:cs) pos
                    | pos == n' + 40 = []
                    | otherwise = ((abs ((pos `mod` 40) - (x c))) < 2) : line' cs' (pos + 1)
                    where
                        c' = head cs
                        cs' = if (time c') > pos + 1 then c:cs else cs

capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = toUpper x : xs

main :: IO ()
main = do
    input :: [Instruction] <- map (read . capitalise) . lines <$> getContents
    let states = runProgram input
    print $ task1 states
    putStrLn $ unlines $ task2 states
