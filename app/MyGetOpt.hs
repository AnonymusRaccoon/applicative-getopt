import Control.Applicative

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
    (<*>) f p = P $ \x -> case parse f x of
                               Nothing -> Nothing
                               Just (y, lo) -> parse (fmap y p) lo

instance Monad Parser where
    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>>=) p f = P $ \x -> case parse p x of
                               Nothing -> Nothing
                               Just (y, lo) -> parse (f y) lo

instance Alternative Parser where
    -- empty :: f a
    empty = P (\x -> Nothing)

    -- (<|>) :: f a -> f a -> f a
    (<|>) a b = P $ \x -> case parse a x of
                               Nothing -> parse b x
                               y -> y

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

--
--data PeutEtre a = Juste a | Rien
--
--instance Functor PeutEtre where
--    -- fmap :: (a -> b) -> f a -> f b
--    fmap _ Rien = Rien
--    fmap f (Juste x) = Juste $ f x
--
--instance Applicative PeutEtre where
--    -- pure :: a -> f a
--    pure x = Juste x
--
--    -- (<*>) :: f (a -> b) -> f a -> f b
--    (<*>) _ Rien = Rien
--    (<*>) Rien _ = Rien
--    (<*>) (Juste f) (Juste x) = Juste $ f x
--
--instance Semigroup PeutEtre where
--    -- (<>) :: a -> a -> a
--    (<>) Rien x = x
--    (<>) x _ = x
--    (<>) Rien Rien = Rien
--
--instance Monad PeutEtre where
--    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
--    (>>=) Rien _ = Rien
--    (>>=) (Juste x) f = f x
--
--instance Alternative PeutEtre where
--    -- empty :: f a
--    empty = Rien
--
--    -- (<|>) :: f a -> f a -> f a
--    (<|>) (Juste a) _ = Juste a
--    (<|>) _ (Juste a) = Juste a
--    (<|>) _ _ = Rien