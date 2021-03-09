module Main where

--import MyGetOpt

data Cell = Empty | Full 
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

run :: Int -> [CellList]
run ri = iterate generate [Full]
    where
        r = getrule ri
        
        generate :: CellList -> CellList
        generate [] = []
        generate (x:[]) = r Empty Empty x : r Empty x Empty : r x Empty Empty : []
        generate (x:y:xs) = r Empty Empty x : r Empty x y : endgen (x:y:xs)

        endgen :: CellList -> CellList
        endgen (x:y:z:xs) = r x y z : endgen (y:z:xs)
        endgen (x:y:[])   = r x y Empty : r y Empty Empty : []
        endgen _          = error "Invalid generator status"

printCells :: Configuration -> [CellList] -> IO()
printCells _ [] = putChar '\n'
printCells config (x:xs) = pl x >> printCells config xs
    where
        pl :: CellList -> IO()
        pl [] = putChar '\n'
        pl (y:ys) = putChar (if y == Empty then ' ' else '*') >> pl ys

main :: IO ()
main = case getConfig of 
            Nothing -> exitFailure(ExitCode 84)
            Just config -> runAndPrint config
    where
        runAndPrint :: Configuration -> IO()
        runAndPrint config = printCells config (post . run (rule config))

        post :: [CellList] -> [CellList]
        post = skipX (start config) . takeX ()

    let cells = run (rule config)
    let fcells = case Main.lines config of
                  Nothing -> cells
                  Just x  -> take x cells
    printCells config fcells
--    print option
--             (  long "rule"
--             <> short "r"
--             <> metavar "RULE"
--             <> help "The rulset used."
--             )

getConfig :: Maybe Configuration
getConfig = Just Configuration 30 (Just 0) (Just 10) (Just 80) (Just 0)


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