import Control.Applicative

newtype Parser a = P (String -> Maybe a)

parse :: Parser a -> String -> Maybe a
parse (P f) x = f x

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap map p = P $ \x -> case parse p x of
                                Nothing -> Nothing
                                Just y -> Just $ map y


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