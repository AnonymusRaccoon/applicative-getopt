{-# LANGUAGE GADTs #-}
module GetOpt.Data (
    Parser(DefParser, OptParser),
    OptionParser,
    Option(..)
) where


data Parser a where 
    DefParser :: a -> Parser a
    OptParser :: Option (a -> b) -> Parser a -> Parser b

-- newtype OptionParser a = OptionParser { parse :: String -> Maybe a }

type OptionParser a = (String -> Maybe a)

data Option a = Option {
    metavar :: String,
    longName :: String,
    shortName :: Char,
    defaultValue :: Maybe a,
    helpMessage :: String,
    parser :: OptionParser a
}