{-# LANGUAGE GADTs #-}
module GetOpt.Parsers where

import GetOpt.Data (Option(..))

data Parser a where
    DefParser :: a -> Parser a
    OptParser :: Option (a -> b) -> Parser a -> Parser b

instance Functor Parser where
    -- fmap (a -> b) -> f a -> f b
    fmap f (DefParser a) = DefParser $ f a
    fmap f (OptParser opt next) = OptParser (fmap (f .) opt) next

instance Applicative Parser where
    -- pure a -> f a
    pure a = DefParser a

    -- (<*>) f (a -> b) -> f a -> f b
    (<*>) (DefParser f) p = f <$> p
    -- (<*>) (OptParser (Option (a -> b -> c)) Parser (a -> b)) -> Parser a -> Parser c
    -- Ret: OptParser (Option ((a, b) -> c)) (Parser (a, b)) :: Parser (a, b)
    (<*>) (OptParser f next) p = OptParser (uncurry <$> f) (fmap (,) next <*> p)