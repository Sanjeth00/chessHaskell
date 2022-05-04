
module Board where

import Piece
import Render
import Data.List (genericIndex, intercalate)

defaultShownBoard :: String
defaultShownBoard = unlines [
    "RBNQKNBR",
    "PPPPPPPP",
    "........",
    "........",
    "........",
    "........",
    "pppppppp",
    "rbnqknbr"
    ]


boardColumnIds :: [Char]
boardColumnIds = ['A'..'H']

boardRowIds :: [Int]
boardRowIds = [8,7..1]

horizChar :: Char
horizChar = '-'

vertChar :: Char
vertChar = '|'

cornerChar :: Char
cornerChar = '+'

newLine :: Char
newLine = '\n'

leftRightPad :: Int
leftRightPad = 1

bordersLen :: Int
bordersLen = 2


outerLineSeparator :: Int -> [Char]
outerLineSeparator lineLen = 
    [newLine] ++ (take lineLen $ repeat horizChar) ++ [newLine]

innerLineSeparator  :: Int -> Int -> [Char]
innerLineSeparator lrPad containedCharLen = 
    [newLine, horizChar] ++ (
        intercalate [cornerChar] $
        take 8 $  
        repeat $ take (lrPad * 2 + containedCharLen) $ (repeat horizChar) 
    )
    ++ [horizChar, newLine]


renderBoard :: [[Square]] -> [[Char]]
renderBoard board =
    let charToShowLen = 1
        -- Row separator
        lineSeparator = outerLineSeparator ((leftRightPad * 2 + charToShowLen + (div bordersLen 2)) * 8 + 1)
        
        -- Row separator cell separators;  E.g., "+---+---+---+etc."
        lineSeparator1 = innerLineSeparator leftRightPad 1
    in 
        concatMap (\row -> [
            (
                if (squareRow $ row !! 0) == 8 then lineSeparator
                else ""
            ) ++
            (concatMap (\square -> (
                    (
                        if (squareColumn square) == 'A' then [vertChar]
                        else ""
                    ) ++
                    (take leftRightPad $ repeat ' ') ++
                    ([squareRenderId square]) ++
                    (take leftRightPad $ repeat ' ') ++
                    [vertChar] ++ 
                    (
                        if (squareColumn square) == 'H' then
                            if (squareRow square) /= 1 then lineSeparator1
                            else lineSeparator
                        else ""
                    )
                    ) :: [Char]
                ) row)
            ]
        ) board


-- renderBoardPositions

renderBoardPositions :: [[Square]] -> [[Char]]
renderBoardPositions board = 
    let algebraicIdLen = 2

        -- Row separator
        lineSeparator = outerLineSeparator ((leftRightPad * 2 + algebraicIdLen + (div bordersLen 2)) * 8 + 1)
        
        -- Row separator with cell separators;  E.g., "+---+---+---+etc."
        lineSeparator1 = innerLineSeparator leftRightPad algebraicIdLen
    in 
        concatMap (\row -> [
            (
                if (squareRow $ row !! 0) == 8 then lineSeparator
                else ""
            ) ++
            (concatMap (\square -> (
                    (
                        if (squareColumn square) == 'A' then [vertChar]
                        else ""
                    ) ++
                    (take leftRightPad $ repeat ' ') ++
                    (squareAlgebraicId square) ++
                    (take leftRightPad $ repeat ' ') ++
                    [vertChar] ++ 
                    (
                        if (squareColumn square) == 'H' then
                            if (squareRow square) /= 1 then lineSeparator1
                            else lineSeparator
                        else ""
                    )
                    ) :: [Char]
                ) row)
            ]
        ) board


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

