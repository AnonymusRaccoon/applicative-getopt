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

