
module Chess where

-- | Positions (Pos) 
-- | game states (State)
-- https://wiki.haskell.org/Learning_Haskell_with_Chess
-- https://www.youtube.com/watch?v=6KkF5-_erns

type Board = [[Square]]

initialBoardStr :: String
initialBoardStr = unlines ["rnbqkbnr",
                            "pppppppp",
                            "********",
                            "********",
                            "********",
                            "********",
                            "PPPPPPPP",
                            "RNBQKBNR"]

brett :: [Char] -> [Char]
brett [] = []
brett (x:xs) = if x == '*' then (' ' : brett xs) else (x : brett xs) 

readBoard :: String -> Board
readBoard = map readRow . lines
    where
        readRow = map readSquare

showBoard :: [[Square]] -> String
showBoard = unlines . map showRow
    where
        showRow = map showSquare

type Square = Maybe Piece

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare a = Just (readPiece a)

data Piece = 
    Piece PColor PType
    deriving (Show)
data PColor = 
    White | Black
    deriving (Show)
data PType = 
    Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show)

-- | Pieces using FEN notation
-- White pieces: "PNBRQK"
-- Black pieces: "pnbrqk"

showPiece :: Piece -> Char
showPiece (Piece White Pawn) = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook) = 'R'
showPiece (Piece White Queen) = 'Q'
showPiece (Piece White King) = 'K'
showPiece (Piece Black Pawn) = 'p'
showPiece (Piece Black Knight) = 'n'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook) = 'r'
showPiece (Piece Black Queen) = 'q'
showPiece (Piece Black King) = 'k'

readPiece :: Char -> Piece
readPiece 'P' = (Piece White Pawn)
readPiece 'N' = (Piece White Knight) 
readPiece 'B' = (Piece White Bishop)
readPiece 'R' = (Piece White Rook)
readPiece 'Q' = (Piece White Queen)
readPiece 'K' =  (Piece White King)
readPiece 'p' = (Piece Black Pawn)
readPiece 'n' =  (Piece Black Knight)
readPiece 'b' =  (Piece Black Bishop)
readPiece 'r' =  (Piece Black Rook) 
readPiece 'q' = (Piece Black Queen) 
readPiece 'k' = (Piece Black King)

clr :: IO()
clr = 
    do 
        putStr "\ESC[2J"
        putStr "\ESC[0:0H"

-- Maybe make own functions for white & black
-- Function for the game

main :: IO()
main = do
    clr
    putStrLn (brett initialBoardStr)
    putStr "White: \n"
    white <- getLine
    case white of
        "quit" -> do
            return ()
        _ -> do
            return ()
    putStr "Black: \n"
    black <- getLine
    case black of
        "quit" -> do
            return ()
        _ -> do
            return ()