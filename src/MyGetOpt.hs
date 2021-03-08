module MyGetOpt where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { parse :: (String -> Maybe (a, String)) }

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f p = Parser $ \x -> case parse p x of
                                   Nothing -> Nothing
                                   Just (y, lo) -> Just (f y, lo)

instance Applicative Parser where
    -- pure :: a -> f a
    pure x = Parser (\lo -> Just (x, lo))

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) (Parser f) (Parser p) = Parser $ \str -> do
        (f, lo) <- f str
        (v, lo1) <- p lo
        return (f v, lo1)

instance Monad Parser where
    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>>=) p f = Parser $ \str -> case parse p str of
                                      Nothing -> Nothing
                                      Just (y, lo) -> parse (f y) lo

instance Alternative Parser where
    -- empty :: f a
    empty = Parser (\_ -> Nothing)

    -- (<|>) :: f a -> f a -> f a
    (<|>) a b = Parser $ \x -> case parse a x of
                                    Nothing -> parse b x
                                    y -> y


char :: Parser Char
char = Parser $ \x -> case x of
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
maybeInt = Parser $ \str -> case parse int str of
                                 Nothing -> Just (Nothing, str)
                                 Just (y, lo)  -> Just (Just y, lo)

token :: Parser a -> Parser a
token p = do
    many $ charIf isSpace
    ret <- p
    many $ charIf isSpace
    return ret


data Option a = Option {
    metavar :: String,
    longName :: String,
    shortName :: Char,
    defaultValue :: Maybe a,
    helpMessage :: String,
    parser :: Parser a
}

meta :: String -> Mod a
meta v = Mod $ \x -> x { metavar = v }

long :: String -> Mod a
long v = Mod $ \x -> x { longName = v }

short :: Char -> Mod a
short v = Mod $ \x -> x { shortName = v }

value :: a -> Mod a
value v = Mod $ \x -> x { defaultValue = Just v }

help :: String -> Mod a
help v = Mod $ \x -> x { helpMessage = v }


newtype Mod a = Mod (Option a -> Option a)

instance Semigroup (Mod a) where
    -- (<>) a -> a -> a
    (<>) (Mod a) (Mod b) = Mod (a . b)

-- If need to compile, comment everything bellow.
option :: Parser Int -> Mod Int -> Parser a
option p (Mod m) = optionParser (m $ def p)
    where
        def = Option "VAR" "" ' ' Nothing "No help message set."

optionParser :: Option a -> Parser a
optionParser _ []


-- TODO Create a type OptionParser witch contains the short & long names & n args parsers.
-- TODO Pattern match for the OptionParser or a default parser in the runParser. Option parser will check named args while the default one will do positional ones.
-- EXAMPLE: https://www.paolocapriotti.com/blog/2012/04/27/applicative-option-parser/

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser ::
