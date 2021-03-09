module Main where

import Lib
import MyGetOpt

data Cell = Empty | Full
newType CellList = CellList { left :: [], middle :: [], right :: [] }

run :: Int -> [CellList]
run _ = []

printCells :: Configuration -> [CellList] -> IO()
printCells _ [] = return
printCells config (x:xs) = pl x >> printCells config xs
    where
        pl cl =

main :: IO ()
main = do
    config = Configuration 30 0 10 80 0
    printCells config $ run (rule config)
--    print option
--             (  long "rule"
--             <> short "r"
--             <> metavar "RULE"
--             <> help "The rulset used."
--             )


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