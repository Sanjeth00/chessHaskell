
module Square where

import Piece
import Render
import Data.List (genericIndex)


type SquareColumn = Char -- Board column - one of [1..8]
type SquareRow = Int -- Board row - one of [A..H]

data SquareColor = SquareBlack | SquareWhite deriving (Show, Eq)

data Square = Square {
    squareColumn :: Char,
    squareRow :: Int,
    squareOccupier :: Maybe Piece,
    squareColor :: SquareColor,
    squareRenderId :: RenderNote,  -- One of 'pbknqr.'
    squareAlgebraicId :: [Char] -- Algebraic notation for square;  E.g. 'A1, B2, E7, etc.'
}
    deriving Show


boardColumnIds :: [Char]
boardColumnIds = ['A'..'H']

boardRowIds :: [Int]
boardRowIds = [8,7..1]

fromRenderNotation :: String -> [[Square]]
fromRenderNotation renderNotation = fromRenderNotation1 renderNotation 0

fromRenderNotation1 :: String -> Int -> [[Square]]
fromRenderNotation1 renderNoteRows colorToggleModifier =
    map (\(row, rowNum) ->
        map (\(renderNote, colInd) ->
            let sColor =
                    squareColorByEvenOdd
                        (colInd + rowNum + colorToggleModifier)
                        SquareBlack
                        SquareWhite
            in squareFromRenderNote
                renderNote
                (genericIndex boardColumnIds (colInd - 1))
                rowNum
                sColor
        ) $ zip row (reverse boardRowIds)
    ) $ zip (lines renderNoteRows) boardRowIds

squareColorByEvenOdd :: Int -> SquareColor -> SquareColor -> SquareColor
squareColorByEvenOdd i a b
    | even i = a
    | otherwise = b

squareFromRenderNote :: RenderNote -> Char -> Int -> SquareColor -> Square
squareFromRenderNote renderNote column row color =
    Square {
        squareColumn = column,
        squareRow = row,
        squareOccupier = piece,
        squareColor = color,
        squareRenderId = maybe '.' pieceRenderId piece,
        squareAlgebraicId = [column] ++ (show row)
        }
    where
        piece = fromRenderNote renderNote
