{-# Language LambdaCase #-}
{-# Language ViewPatterns #-}
import Control.Applicative
import Control.Monad (liftM2)
import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.Maybe (fromJust, fromMaybe)

data Value = ListValue [Value] | IntValue Int
    deriving (Show)

instance Eq Value where
    (IntValue a) == (IntValue b)        = a == b
    (ListValue xs) == (ListValue ys)    = (length xs == length ys) && all id (zipWith (==) xs ys)
    lv@(ListValue _) == iv@(IntValue _) = lv == (ListValue [iv])
    iv@(IntValue _) == lv@(ListValue _) = lv == (ListValue [iv])

instance Ord Value where
    compare (IntValue a) (IntValue b)        = compare a b
    compare (ListValue xs) (ListValue ys)    = fromMaybe (compare (length xs) (length ys)) (find (/= EQ) $ zipWith compare xs ys)
    compare lv@(ListValue _) iv@(IntValue _) = compare lv (ListValue [iv])
    compare iv@(IntValue _) lv@(ListValue _) = compare (ListValue [iv]) lv

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (Parser p) <*> (Parser q) = Parser $ \s -> do
        (f, s') <- p s
        (a, s'') <- q s'
        Just (f a, s'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p) <|> (Parser q) = Parser $ liftM2 (<|>) p q

charP :: Char -> Parser Char
charP c = Parser $ \case
    y:ys -> if c == y then Just (c, ys) else Nothing
    ""   -> Nothing

predP :: (Char -> Bool) -> Parser Char
predP pred = Parser f
    where
        f []            = Nothing
        f (x:xs)
            | pred x    = Just (x, xs)
            | otherwise = Nothing

intP :: Parser Int
intP = read <$> some (predP isDigit)

separatedP :: Parser a -> Parser b -> Parser [b]
separatedP separator p = (:) <$> p <*> many (separator *> p)

intValueP :: Parser Value
intValueP = IntValue <$> intP

listValueP :: Parser Value
listValueP = charP '[' *> (ListValue <$> elements) <* charP ']'
    where
        elements = (separatedP (charP ',') valueP) <|> pure []

valueP :: Parser Value
valueP = listValueP <|> intValueP

readPacket :: String -> Value
readPacket = fromJust . fmap fst . runParser valueP

readPairs :: [String] -> [(Value, Value)]
readPairs [] = []
readPairs ("":xs) = readPairs xs
readPairs xs = uncurry (:) (bimap ((\(x:y:[]) -> (x, y)) . fmap readPacket) readPairs $ span (not . null) xs)

flattenPairs :: [(a, a)] -> [a]
flattenPairs [] = []
flattenPairs ((a, b):xs) = a : b : flattenPairs xs

main :: IO ()
main = do
    input <- readPairs . lines <$> getContents
    let input' = ListValue [ListValue [IntValue 2]] : ListValue [ListValue [IntValue 6]] : flattenPairs input
    print $ sum $ map fst $ filter snd $ (second $ (/= GT) . uncurry compare) <$> (zip [1..] input)
    print $ foldl (*) 1 $ map fst $ filter (fst . snd) $ zip [1..] (sortOn snd $ zip ([True, True] ++ (repeat False)) input')