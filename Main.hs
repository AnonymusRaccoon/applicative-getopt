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
            <> unset 30
         )
         <*> option auto (
               long "start"
            <> short 's'
            <> meta "START"
            <> help "At witch line should we start"
            <> value 0
         )
         <*> option (Just . auto) (
               long "lines"
            <> short 'l'
            <> meta "LINES"
            <> help "The number of lines to print"
            <> value Nothing
         )
         <*> option auto (
               long "window"
            <> short 'w'
            <> meta "WINDOW"
            <> help "The size of the window (how long a line should be)"
            <> value 80
         )
         <*> option auto (
               long "move"
            <> short 'm'
            <> meta "MOVE"
            <> help "Offset every lines."
            <> value 0
         )



data Configuration = Configuration {
    rule :: Int,
    start :: Int,
    lines :: Maybe Int,
    window :: Int,
    move :: Int
} deriving Show

