{-# Language ViewPatterns #-}
-- requires package unordered-containers
import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as Hm

data Node = File { size :: Int } | Directory { contents :: Hm.HashMap String Node } deriving(Show)
data Instruction = Ls | Cd { path :: String }

totalSize :: Node -> Int
totalSize (File size) = size
totalSize (Directory tree) = foldl (\b a -> b + (totalSize a)) 0 tree 

toListOfSizes :: Node -> [Int]
toListOfSizes = map snd . filter (not . fst) . toListOfSizes'
    where
        toListOfSizes' :: Node -> [(Bool, Int)]
        toListOfSizes' (File size) = [(True, size)]
        toListOfSizes' (Directory tree) = (False, sum $ snd <$> filter fst recRes) : recRes
            where
                recRes = concatMap (toListOfSizes' . snd) (Hm.toList tree)

task1 :: Node -> Int
task1 = sum . filter (<= 100000) . toListOfSizes

task2 :: Node -> Int
task2 node = (minimum . filter (>= 30000000 - (70000000 - (totalSize node))) . toListOfSizes) node

getInstruction :: String -> Instruction
getInstruction (tail . words -> tokens) = case (head tokens) of
    "cd" -> Cd (last tokens)
    "ls" -> Ls
    _    -> error $ "Unknown instruction: " ++ (last tokens)

getNode :: String -> (String, Node)
getNode (words -> tokens) = (name
    , case marker of
        "dir" -> Directory Hm.empty
        _     -> File (read marker)
    )
    where
        marker = head tokens
        name   = last tokens

buildFs :: [String] -> Node -> ([String], Node)
buildFs [] node = ([], node)
buildFs (x:xs) node@(Directory tree) = case (getInstruction x) of
    Cd dir' -> case dir' of
        ".." -> (xs, node)
        dir  -> uncurry buildFs $ second (\x -> Directory $ Hm.insert dir x tree) $ buildFs xs (Hm.lookupDefault (Directory Hm.empty) dir tree)
    Ls     -> buildFs xs' (Directory $ (Hm.fromList (getNode <$> nodes)) `Hm.union` tree)
    where
        (nodes, xs') = break ((== '$') . head) xs

main :: IO ()
main = do
    input <- lines <$> getContents
    let (_, fs@(Directory tree)) = buildFs input (Directory Hm.empty)
    print $ task1 fs
    print $ task2 fs