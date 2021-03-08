module Main where

import Lib
import MyGetOpt

main :: IO ()
main = print option
             (  long "rule"
             <> short "r"
             <> metavar "RULE"
             <> help "The rulset used."
             )


data Configuration = Configuration {
    rule :: Int,
    start :: Maybe Int,
    lines :: Maybe Int,
    window :: Maybe Int,
    move :: Maybe Int
} deriving Show

--config :: Parser Configuration
--config = Configuration
--    <$> option
--        (  long "rule"
--        <> short "r"
--        <> metavar "RULE"
--        <> help "The rulset used."
--        )


--defaultConfiguration = Configuration {
--    rule = 0,
--    start = Just 0,
--    Main.lines = Nothing,
--    window = Just 80,
--    move = Just 0
--}