
module Render where

import Data.Char (toLower)

type RenderNote = Char -- Render notation symbol
type RenderNotation = [RenderNote]

isValidPieceRenderNote :: RenderNote -> Bool
isValidPieceRenderNote x = elem (toLower x) validPieceRenderNotes

validPieceRenderNotes :: [RenderNote]
validPieceRenderNotes = init validRenderNotes

validRenderNotes :: [RenderNote]
validRenderNotes = "pnbrqk."