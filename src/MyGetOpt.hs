{-# LANGUAGE LambdaCase #-}
module MyGetOpt where

-- import Control.Applicative ( Alternative(..) )
-- import Data.Char ( isSpace, isDigit, isAlphaNum )

-- data Parser a where 
--     Parser :: String -> Maybe (a, String)
--     OptParser :: Option (a -> b) -> Parser a -> Parser b

-- instance Functor Parser where
--     -- fmap :: (a -> b) -> f a -> f b
--     fmap f p@(Parser _) = Parser $ \str -> do
--         (v, lo) <- p str
--         return (f v, lo)
--     fmap f (OptParser opt p) = OptParser opt (fmap f p)

-- instance Applicative Parser where
--     -- pure :: a -> f a
--     pure x = Parser (\lo -> Just (x, lo))

--     -- (<*>) :: f (a -> b) -> f a -> f b
--     (<*>) (Parser f) p = fmap f p
--     (<*>) (OptParser opt f) p = OptParser

-- instance Monad Parser where
--     -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
--     (>>=) p f = Parser $ \str -> case parse p str of
--                                       Nothing -> Nothing
--                                       Just (y, lo) -> parse (f y) lo

-- instance Alternative Parser where
--     -- empty :: f a
--     empty = Parser $ const Nothing

--     -- (<|>) :: f a -> f a -> f a
--     (<|>) a b = Parser $ \x -> case parse a x of
--                                     Nothing -> parse b x
--                                     y -> y


-- char :: Parser Char
-- char = Parser $ \case
--                     []     -> Nothing
--                     (x:xs) -> Just (x, xs)

-- charIf :: (Char -> Bool) -> Parser Char
-- charIf f = do
--     x <- char
--     if f x then return x else empty

-- alphaNum :: Parser Char
-- alphaNum = charIf isAlphaNum

-- digit :: Parser Char
-- digit = charIf isDigit

-- num :: Parser Int
-- num = do
--     x <- some digit
--     return $ read x

-- int :: Parser Int
-- int =
--     do
--         charIf $ \x -> x == '-'
--         x <- num
--         return (-x)
--     <|>
--         num

-- maybeInt :: Parser (Maybe Int)
-- maybeInt = Parser $ \str -> case parse int str of
--                                  Nothing -> Just (Nothing, str)
--                                  Just (y, lo)  -> Just (Just y, lo)

-- token :: Parser a -> Parser a
-- token p = do
--     many $ charIf isSpace
--     ret <- p
--     many $ charIf isSpace
--     return ret


-- data Option a = Option {
--     metavar :: String,
--     longName :: String,
--     shortName :: Char,
--     defaultValue :: Maybe a,
--     helpMessage :: String,
--     parser :: Parser a
-- }

-- meta :: String -> Mod a
-- meta v = Mod $ \x -> x { metavar = v }

-- long :: String -> Mod a
-- long v = Mod $ \x -> x { longName = v }

-- short :: Char -> Mod a
-- short v = Mod $ \x -> x { shortName = v }

-- value :: a -> Mod a
-- value v = Mod $ \x -> x { defaultValue = Just v }

-- help :: String -> Mod a
-- help v = Mod $ \x -> x { helpMessage = v }


-- newtype Mod a = Mod (Option a -> Option a)

-- instance Semigroup (Mod a) where
--     -- (<>) a -> a -> a
--     (<>) (Mod a) (Mod b) = Mod (a . b)

-- option :: Parser a -> Mod a -> Parser a
-- option p (Mod m) = OptParser (m $ def p)
--    where
--        def = Option "VAR" "" ' ' Nothing "No help message set."

-- -- TODO Create a type OptionParser witch contains the short & long names & n args parsers.
-- -- TODO Pattern match for the OptionParser or a default parser in the runParser. Option parser will check named args while the default one will do positional ones.
-- -- EXAMPLE: https://www.paolocapriotti.com/blog/2012/04/27/applicative-option-parser/

-- runParser :: Parser a -> [String] -> Maybe (a, [String])
-- runParser (OptParser opt nextp) (o:xs)
--     | optionMatch opt o = runOption opt xs
--     | otherwise         = runParser (OptParser opt NEXTPARSER) (runParser nextp o:xs)

--     where
--         optionMatch :: Option a -> String -> Bool
--         optionMatch opt [d, s] = d == '-' && s == shortName opt
--         optionMatch opt (d:d1:l) = d == '-' && d1 ==  '-' && l == longName opt

--         runOption :: Option a -> [String] -> Maybe (a, [String])
--         runOption opt (value:args) = case parse opt value of
--                                           Just (out, []) -> Just (out, args)
--                                           _              -> Nothing

--         skipOption :: Option a -> Parser b -> [String] -> Maybe (a, [String])
--         skipOption skipped nextP args