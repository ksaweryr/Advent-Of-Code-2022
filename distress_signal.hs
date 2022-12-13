{-# Language LambdaCase, ViewPatterns #-}
import Control.Applicative
import Control.Monad (liftM2)
import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.Maybe (fromJust, fromMaybe)

data Value = ListValue [Value] | IntValue Int

toListValue :: Value -> Value
toListValue lv@(ListValue _) = lv
toListValue iv@(IntValue _)  = ListValue [iv]

instance Eq Value where
    a == b = compare a b == EQ

instance Ord Value where
    compare (IntValue a) (IntValue b) = compare a b
    compare (toListValue -> ListValue xs) (toListValue -> ListValue ys) = compare xs ys

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
    x:xs -> if c == x then Just (c, xs) else Nothing
    ""   -> Nothing

predP :: (Char -> Bool) -> Parser Char
predP pred = Parser $ \case
    x:xs -> if pred x then Just (x, xs) else Nothing
    ""   -> Nothing

intP :: Parser Int
intP = read <$> some (predP isDigit)

separatedP :: Parser a -> Parser b -> Parser [b]
separatedP separator p = (:) <$> p <*> many (separator *> p)

intValueP :: Parser Value
intValueP = IntValue <$> intP

listValueP :: Parser Value
listValueP = charP '[' *> (ListValue <$> elements) <* charP ']'
    where
        elements = separatedP (charP ',') valueP <|> pure []

valueP :: Parser Value
valueP = listValueP <|> intValueP

readPacket :: String -> Value
readPacket = fst . fromJust . runParser valueP

readPairs :: [String] -> [(Value, Value)]
readPairs [] = []
readPairs ("":xs) = readPairs xs
readPairs xs = uncurry (:) (bimap ((\[x,y] -> (x, y)) . fmap readPacket) readPairs $ break null xs)

flattenPairs :: [(a, a)] -> [a]
flattenPairs [] = []
flattenPairs ((a, b):xs) = a : b : flattenPairs xs

main :: IO ()
main = do
    input <- readPairs . lines <$> getContents
    let input' = ListValue [ListValue [IntValue 2]] : ListValue [ListValue [IntValue 6]] : flattenPairs input
    print $ sum $ map fst $ filter (uncurry (<=) . snd) $ zip [1..] input
    print $ product $ map fst $ filter (fst . snd) $ zip [1..] (sortOn snd $ zip ([True, True] ++ repeat False) input')
