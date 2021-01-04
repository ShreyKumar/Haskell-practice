module Lib
    (formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
    , findWordInLinePrefix
    , skew
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , cell2char
    , Cell(Cell, Indent)
    , Game(gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , completed
    , playGame
    , formatGame
    , makeRandomGrid
    , fillInBlanks
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Char (toLower)
import System.Random
import qualified Data.Map as M

data Game = Game {
                gameGrid :: Grid Cell,
                gameWords :: M.Map String (Maybe [Cell])
            }
            deriving Show

data Cell = Cell (Integer, Integer) Char
            | Indent
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
    let gwc = gridWithCoords grid
        tuplify word = (word, Nothing)
        list = map tuplify words
        dict = M.fromList list
    in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
    let grid = gameGrid game
        foundWord = findWord grid word
    in case foundWord of
        Nothing -> game
        Just cs ->
            let dict = gameWords game
                newDict = M.insert word foundWord dict
            in game { gameWords = newDict }

formatGame :: Game -> String
formatGame game = formatGameGrid game ++ "\n\n" ++ (show $ score game) ++ "/" ++ (show $ totalWords game)

makeRandomGrid gen =
    let (gen1, gen2) = split gen
        row = randomRs ('A', 'Z') gen1
    in row : makeRandomGrid gen2

fillInBlanks gen grid =
    let r = makeRandomGrid gen
        fill '_' r = r
        fill c _ = c
    in zipOverGridWith fill grid r 


zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid


outputGrid :: Grid Cell -> IO()
outputGrid grid = putStrLn (formatGrid grid)

formatGameGrid game =
    let grid = gameGrid game
        dict = gameWords game :: M.Map String (Maybe [Cell])
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell =
            let char = cell2char cell
            in if cell `elem` cellSet then char else toLower char
        charGrid = mapOverGrid formatCell grid
    in unlines charGrid

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c 
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid =
    let horizontal = grid
        reverseHorizontal = map reverse horizontal
        vertical = transpose grid
        reverseVertical = map reverse vertical
        diagonal1 = transpose (skew horizontal)
        diagonal2 = transpose (skew (map reverse horizontal))
        lines = horizontal ++ reverseHorizontal ++ vertical ++ reverseVertical ++ diagonal1 ++ diagonal2 ++ map reverse diagonal1 ++ map reverse diagonal2
    in lines

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines = getLines grid
        foundWords = map (findWordInLine word) lines
    in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords 

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
    let found = findWordInLinePrefix [] word line
    in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

findWordInLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInLinePrefix acc (x:xs) (c:cs)
    | x == cell2char c = findWordInLinePrefix (c : acc) xs cs
findWordInLinePrefix acc [] _ = Just $ reverse acc
findWordInLinePrefix _ _ _ = Nothing