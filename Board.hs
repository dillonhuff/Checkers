module Board() where

import Data.List as L
import Data.Map as M

data Square = S Int Int

instance Eq Square where
	(S a b) == (S c d) = a == c && b == d
	
instance Ord Square where
	(S a b) <= (S c d) = a < c || (a == c && b <= d)

data Piece = Empty | P Player PieceType

instance Show Piece where
	show Empty = "  "
	show (P color pieceType) = show color ++ show pieceType

data Player = Red | Black

instance Show Player where
	show Red = "R"
	show Black = "B"

data PieceType = Reg | King

instance Show PieceType where
	show Reg = " "
	show King = "K"

data Move = M [Shift]

data Shift = Push Square Square | Jump Square Square | Promotion

class Board b where
	s ::  b -> Int -> Int -> Piece
	legalMoves :: b -> Player -> [Move]
	move :: b -> Move -> b

data MapBoard = MapB (Map Square Piece)

instance Show MapBoard where
	show = mBShow
	
mBShow :: MapBoard -> String
mBShow (MapB squaresToPieces) = showRows (assocs squaresToPieces)

showRows :: [(Square, Piece)] -> String
showRows [] = ""
showRows rows = (showRow $ (take 4 rows)) ++ (showRows (drop 4 rows))

showRow :: [(Square, Piece)] -> [Char]
showRow ((S r c, piece):rest) = case mod r 2 of
	0 -> rowStr ++ "__\n"
	1 ->  "__" ++ rowStr ++ "\n"
	where
		rowStr = withEmptySpaces ((S r c, piece):rest)
	
withEmptySpaces :: [(Square, Piece)] -> [Char]
withEmptySpaces row = concat $ ((intersperse "__" (L.map show pieces)))
	where
		pieces = L.map snd row

instance Board MapBoard where
	s = mBoardS
	legalMoves = mBLegalMoves
	move = mbMove
	
startingBoard :: MapBoard
startingBoard = MapB $ fromList $ L.map startingSquare legalPositions
	where legalPositions = [(x, y) | x <- [1..8], y <- [1..8],
		(mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]
	
startingSquare :: (Int, Int) -> (Square, Piece)
startingSquare (row, col) | row <= 3 = (S row col, P Red Reg)
						  | row >= 6 = (S row col, P Black Reg)
						  | otherwise = (S row col, Empty)
	
mBoardS :: MapBoard -> Int -> Int -> Piece
mBoardS (MapB m) a b = case M.lookup (S a b) m of
	Just piece -> piece
	Nothing -> Empty
	

mBLegalMoves :: MapBoard -> Player -> [Move]
mBLegalMoves board player = [M [Promotion]]

mbMove :: MapBoard -> Move -> MapBoard
mbMove board move = board