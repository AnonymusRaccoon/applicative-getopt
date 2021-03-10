module GetOpt.Data (
    OptionParser,
    Option(..)
) where

type OptionParser a = (String -> Maybe a)

data Option a = Option {
    metavar :: String,
    longName :: String,
    shortName :: Char,
    defaultValue :: Maybe a,
    helpMessage :: String,
    parser :: OptionParser a
}

instance Functor Option where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Option mv l s dv hm p) = Option mv l s (fmap f dv) hm (fmap f . p)