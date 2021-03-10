module Main where

import GetOpt
import System.Environment (getArgs)

main :: IO()
main = do
    args <- getArgs
    print $ getOpt getParser args


getParser :: Parser Configuration
getParser = Configuration
         <$> option auto (
               long "rule"
            <> short 'r'
            <> meta "RULE"
            <> help "The rulset used." 
         )
         <*> option auto (
               long "start"
            <> short 's'
            <> meta "START"
            <> help "At witch line should we start"
            <> value 0
         )



data Configuration = Configuration {
    rule :: Int,
    start :: Int
    -- lines :: Maybe Int,
    -- window :: Int,
    -- move :: Int
} deriving Show

