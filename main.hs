
module Main where

import Data.Char (isUpper, toLower)
import Board



main :: IO ()
main = do
    putStrLn "\nBoard Positions"
    putStr $ concat $ renderBoardPositions $ fromRenderNotation defaultShownBoard
    putStrLn "\nRendered Board (with render-note-ids (lower case is 'white' uppercase is 'black'))"
    putStr $ concat $ renderBoard $ fromRenderNotation defaultShownBoard
    return ()