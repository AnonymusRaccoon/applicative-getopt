import Control.Applicative
import Data.Char

newtype Parser a = P { parse :: (String -> Maybe (a, String)) }

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f p = P $ \x -> case parse p x of
                              Nothing -> Nothing
                              Just (y, lo) -> Just (f y, lo)

instance Applicative Parser where
    -- pure :: a -> f a
    pure x = P (\lo -> Just (x, lo))

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) (P f) (P p) = P $ \str -> do
        (f, lo) <- f str
        (v, lo1) <- p lo
        return (f v, lo1)

instance Monad Parser where
    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>>=) p f = P $ \str -> case parse p str of
                               Nothing -> Nothing
                               Just (y, lo) -> parse (f y) lo

instance Alternative Parser where
    -- empty :: f a
    empty = P (\_ -> Nothing)

    -- (<|>) :: f a -> f a -> f a
    (<|>) a b = P $ \x -> case parse a x of
                               Nothing -> parse b x
                               y -> y


char :: Parser Char
char = P $ \x -> case x of
                      [] -> Nothing
                      (x:xs) -> Just (x, xs)

charIf :: (Char -> Bool) -> Parser Char
charIf f = do
    x <- char
    if f x then return x else empty

alphaNum :: Parser Char
alphaNum = charIf isAlphaNum

digit :: Parser Char
digit = charIf isDigit

num :: Parser Int
num = do
    x <- some digit
    return $ read x

int :: Parser Int
int =
    do
        charIf $ \x -> x == '-'
        x <- num
        return (-x)
    <|>
        num

maybeInt :: Parser (Maybe Int)
maybeInt = P $ \str -> case parse int str of
                          Nothing -> Just (Nothing, str)
                          Just (y, lo)  -> Just (Just y, lo)

token :: Parser a -> Parser a
token p = do
    many $ charIf isSpace
    ret <- p
    many $ charIf isSpace
    return ret

data Configuration = Configuration {
    rule :: Int,
    start :: Maybe Int,
    lines :: Maybe Int,
    window :: Maybe Int,
    move :: Maybe Int
} deriving Show

defaultConfiguration = Configuration {
    rule = 0,
    start = Just 0,
    Main.lines = Nothing,
    window = Just 80,
    move = Just 0
}