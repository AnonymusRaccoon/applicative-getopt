{-# LANGUAGE GADTs #-}
module GetOpt (
    getOpt,
    GetOpt.Parsers.Parser(..),
    GetOpt.Options.help,
    GetOpt.Options.long,
    GetOpt.Options.short,
    GetOpt.Options.value,
    GetOpt.Options.meta,
    GetOpt.Options.option,
    GetOpt.Options.auto,
) where

import GetOpt.Data
import GetOpt.Options
import GetOpt.Parsers

getOpt :: Parser a -> [String] -> Maybe (a, [String])
getOpt (DefParser a) args = Just (a, args)
getOpt p args = case runParser p args of
                     Just (p1, args1) -> getOpt p1 args1
                     Nothing          -> Nothing

runParser :: Parser a -> [String] -> Maybe (Parser a, [String])
runParser p@(DefParser _) args = Just (p, args)
runParser (OptParser _ _) [] = Nothing
runParser (OptParser _ _) [_] = Nothing -- TODO remove this and support default values
runParser (OptParser opt next) (identifier:arg:args)
    | optionMatch opt identifier = do
        ret <- parser opt arg
        return (fmap ret next, args)
    | otherwise                  = do
        (nextP, newArgs) <- runParser next (identifier:args)
        return (OptParser opt nextP, newArgs)