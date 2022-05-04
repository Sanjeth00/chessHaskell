
module Main where

import Data.Char (isUpper, toLower)
import Board
import Square

main :: IO ()
main = do
    putStrLn "\nBoard Positions"
    putStr $ concat $ renderBoardPositions $ fromRenderNotation defaultShownBoard
    putStrLn "\nRendered Board (lower case is 'white' uppercase is 'black')"
    putStr $ concat $ renderBoard $ fromRenderNotation defaultShownBoard
    putStr "\nWhite to move: "
    white <- getLine
    case white of
        _ -> do
            putStrLn "Invalid! Try a legal move..."
    putStr "\nBlack to move"
    black <- getLine
    case black of
        _ -> do
            putStrLn "Invalid! Try a legal move..."
    return ()