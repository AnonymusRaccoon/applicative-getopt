import Control.Applicative

newtype Parser a = P { parse :: (String -> Maybe (a, String)) }

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f p = P $ \x -> case parse p x of
                              Nothing -> Nothing
                              Just (y, lo) -> Just $ (f y, lo)

instance Applicative Parser where
    -- pure :: a -> f a
    pure x = P (\lo -> Just (x, lo))

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) f p = P $ \x -> case parse f x of
                               Nothing -> Nothing
                               Just (y, lo) -> parse (fmap y p) lo

instance Alternative Parser where
    -- empty :: f a
    empty = P (\x -> Nothing)

    -- (<|>) :: f a -> f a -> f a
  --  (<|>) a b = P $ \x -> case parse a x of
    --                           Nothing -> b
      --                         _ -> a

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