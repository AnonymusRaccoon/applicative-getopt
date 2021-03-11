{-# LANGUAGE GADTs #-}
module GetOpt (
    getOpt,
    GetOpt.Parsers.Parser(..),
    GetOpt.Options.help,
    GetOpt.Options.long,
    GetOpt.Options.short,
    GetOpt.Options.value,
    GetOpt.Options.meta,
    GetOpt.Options.unset,
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
runParser p@(DefParser _) args = Nothing
runParser (OptParser opt next) [] = do
    def <- defaultValue opt
    return (fmap def next, [])
runParser (OptParser opt next) (identifier:args)
    | optionMatch opt identifier = do
        (ret, lo) <- getArg opt args
        return (fmap ret next, lo)
    | otherwise = do
        (nextP, newArgs) <- runParser next (identifier:args)
        return (OptParser opt nextP, newArgs)
    
    where
        getArg :: Option a -> [String] -> Maybe (a, [String])
        getArg opt (arg:args)
            | head arg /= '-' = do 
                ret <- parser opt arg
                return (ret, args)
            | otherwise = do
                ret <- unsetValue opt
                return (ret, arg:args)
        getArg opt args = do
            ret <- unsetValue opt
            return (ret, args)
