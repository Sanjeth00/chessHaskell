
import Data.List (genericIndex, intercalate)
import Data.Char (isUpper, toLower)

-- | Positions (Pos) 
-- | game states (State)
-- https://wiki.haskell.org/Learning_Haskell_with_Chess

-- Square

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

-- Square


-- Piece

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data PieceColor = White | Black deriving (Show, Eq)

data Piece = Piece {
    pieceColor :: PieceColor,
    pieceType :: PieceType,
    pieceRenderId :: RenderNote
}
    deriving Show

renderNotesAndPieceTypes :: [(RenderNote, PieceType)]
renderNotesAndPieceTypes = [
    ('p', Pawn),
    ('n', Knight),
    ('b', Bishop),
    ('r', Rook),
    ('q', Queen),
    ('k', King)
    ]

getColorFromRenderNote :: RenderNote -> PieceColor
getColorFromRenderNote rId = if isUpper rId then Black else White

getPieceTypeByRenderNote :: RenderNote -> Maybe PieceType
getPieceTypeByRenderNote rId = lookup (toLower rId) renderNotesAndPieceTypes


fromRenderNote :: RenderNote -> Maybe Piece
fromRenderNote rId =
    if ((isValidPieceRenderNote rId) == False) then Nothing
    else maybe
        Nothing
        (\_pieceType -> Just $ Piece{
            pieceColor=(getColorFromRenderNote rId),
            pieceType=_pieceType,
            pieceRenderId=rId
        })
        (getPieceTypeByRenderNote rId)

-- Piece

-- RenderNote

type RenderNote = Char -- Render notation symbol
type RenderNotation = [RenderNote]

isValidPieceRenderNote :: RenderNote -> Bool
isValidPieceRenderNote x = elem (toLower x) validPieceRenderNotes

validPieceRenderNotes :: [RenderNote]
validPieceRenderNotes = init validRenderNotes

validRenderNotes :: [RenderNote]
validRenderNotes = "pnbrqk."

-- RenderNote





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

-- Renderer

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




main :: IO ()
main = do
    putStrLn "\nBoard Positions"
    putStr $ concat $ renderBoardPositions $ fromRenderNotation defaultShownBoard
    putStrLn "\nRendered Board (with render-note-ids (lower case is 'white' uppercase is 'black'))"
    putStr $ concat $ renderBoard $ fromRenderNotation defaultShownBoard
    return ()