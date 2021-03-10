{-# LANGUAGE GADTs #-}
module GetOpt where

import GetOpt.Data
import GetOpt.Options
import GetOpt.Parsers

getOpt :: Parser a -> [String] -> Maybe (a, [String])
getOpt (DefParser a) args = Just (a, args)
getOpt p args = case runParser p args of
                     Just (p1, args1) -> getOpt p1 args1
                     Nothing          -> Nothing
    where
        runParser :: Parser a -> [String] -> Maybe (Parser a, [String])
        runParser (OptParser opt next) (identifier:arg:args)
            | optionMatch opt identifier = do
                ret <- parser opt arg
                return (fmap ret next, args)
            | otherwise                  = do
                (nextP, newArgs) <- runParser next (identifier:args)
                return (OptParser opt nextP, newArgs)
        runParser p@(DefParser _) args = Just (p, args)