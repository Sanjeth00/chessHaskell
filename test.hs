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
    putStrLn "Here are som tips: \nPress r for rules\nPress q to quit\nWhen moving a piece you have to write e2e4 (This will move pawn at E2 to E4)"
    white

white :: IO ()
white = do
    putStr "White to move: "
    white <- getLine
    case white of
        "r" -> do
            putStrLn "Coming soon"
        "q" -> do
            return ()
        _ -> do
            -- move piece
            -- if this posistion is not a "." and the piece can actually move there without rule violation, piece will move
            -- Printing new board everytime? Changing the original one? maybe make the board to txt file which can change and is read by a function?
            return ()