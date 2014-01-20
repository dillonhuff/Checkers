module Board() where

data Square = Empty | Piece Player PieceType

data Player = Red | Black

data PieceType = Reg | King

data Move = M [(Int, Int)]

class Board b where
	--Gets the contents of a square
	s :: (Integral a) => b -> a -> a -> Square
	legalMoves :: b -> Player -> [Move]
	move :: b -> Move -> b
	
