module Main where

import System.Exit (exitWith, ExitCode (ExitFailure))
--import MyGetOpt

data Cell = Empty | Full | Pad
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

type CellList = [Cell]

type CellGen = Cell -> Cell -> Cell -> Cell


rule30 :: Cell -> Cell -> Cell -> Cell
rule30 Full  Empty Empty = Full
rule30 Empty Full  Full  = Full
rule30 Empty Full  Empty = Full
rule30 Empty Empty Full  = Full
rule30 _     _     _     = Empty

getrule :: Int -> CellGen
getrule 30 = rule30
getrule r = error $ "Unsupported rule" ++ (show r)
--getrule  90 = rule90
--getrule 110 = rule110

generate :: Int -> [CellList]
generate ri = iterate gen [Full]
    where
        r = getrule ri

        gen :: CellList -> CellList
        gen [] = []
        gen (x:[]) = r Empty Empty x : r Empty x Empty : r x Empty Empty : []
        gen (x:y:xs) = r Empty Empty x : r Empty x y : endgen (x:y:xs)

        endgen :: CellList -> CellList
        endgen (x:y:z:xs) = r x y z : endgen (y:z:xs)
        endgen (x:y:[])   = r x y Empty : r y Empty Empty : []
        endgen _          = error "Invalid generator status"

printCells :: Configuration -> [CellList] -> IO()
printCells _ [] = putChar '\n'
printCells config (x:xs) = plist x >> printCells config xs
    where
        plist :: CellList -> IO()
        plist = pl
        -- plist cl
        --     | length cl < 
        
        pl :: CellList -> IO()
        pl [] = putChar '\n'
        pl (y:ys) = putChar (toChar y) >> pl ys

        toChar :: Cell -> Char
        toChar Empty = ' '
        toChar Pad = ' '
        toChar Full = '*'

main :: IO ()
main = case getConfig of 
            Nothing -> exitWith (ExitFailure 84)
            Just config -> printCells config (post config . generate $ rule config)
    where
        post :: Configuration -> [CellList] -> [CellList]
        post config = drop (start config)
                    . runMaybe (Main.lines config) take
                    . map (rotate $ move config)
                    . map (align $ window config)

        runMaybe :: Maybe a -> (a -> b -> b) -> b -> b
        runMaybe Nothing _ v = v
        runMaybe (Just n) f v = f n v
        
        align :: Int -> CellList -> CellList
        align win cl
            | len <= win = pad <> cl <> pad
            | otherwise  = take win . drop (len `div` 2 - win `div` 2) $ cl
            where
                len = length cl
                pad = replicate ((win - len) `div` 2) Pad
        
        rotate :: Int -> [a] -> [a]
        rotate _ [] = []
        rotate _ [x] = [x]
        rotate 0 xs = xs
        rotate n xs
            |  n < 0 = reverse (rotate (n * (-1)) (reverse xs))
            |  otherwise = zipWith const (drop n (cycle xs)) xs


getConfig :: Maybe Configuration
getConfig = Just $ Configuration 30 0 (Just 20) 80 0


data Configuration = Configuration {
    rule :: Int,
    start :: Int,
    lines :: Maybe Int,
    window :: Int,
    move :: Int
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