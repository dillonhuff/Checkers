module Board() where

data Square = S Piece Int Int

instance Show Square where
	show (S p _ _) = show p

instance Eq Square where
	(S _ a b) == (S _ c d) = a == c && b == d
	
instance Ord Square where
	(S _ a b) <= (S _ c d) = a < c || (a == c && b <= d)

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

data Move = M [(Int, Int)]

class Board b where
	s :: (Integral a) => b -> a -> a -> Square
	legalMoves :: b -> Player -> [Move]
	move :: b -> Move -> b

data MapBoard = MapB [Square]

instance Board MapBoard where
	s = mBoardS
	legalMoves = mBLegalMoves
	move = mbMove
	
mBoardS :: (Integral a) => mapBoard -> a -> a -> Square
mBoardS board a b = S Empty 1 2

mBLegalMoves :: mapBoard -> Player -> [Move]
mBLegalMoves board player = [M [(1, 2), (2, 3)]]

mbMove :: mapBoard -> Move -> mapBoard
mbMove board move = board