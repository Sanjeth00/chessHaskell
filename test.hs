module Test where

import Data.Char (isUpper, toLower)
import Board
import Square

-- test to move a Piece


test :: IO ()
test = do
    putStrLn "\nWelcome to chess!"
    putStrLn "Press 1 for single player or press 2 for multiplayer"
    game <- getLine
    case game of
        "1" -> do
            putStrLn "Coming soon..."
            return ()
        "2" -> do
            putStrLn "Basic rules"
            spill
        _ -> do
            putStrLn "Invalid!"
            test

spill :: IO ()
spill = do
    putStr $ concat $ renderBoardPositions $ fromRenderNotation defaultShownBoard
    putStr $ concat $ renderBoard $ fromRenderNotation defaultShownBoard
    putStr "\n"
    putStr "White to move: "
    white <- getLine
    case white of
        _ -> do
            -- move piece
            return ()