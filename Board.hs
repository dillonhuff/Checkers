module Board() where

import Data.List as L
import Data.Map as M

data Square = S Int Int

instance Eq Square where
	(S a b) == (S c d) = a == c && b == d
	
row :: Square -> Int
row (S a _) = a

col :: Square -> Int
col (S _ b) = b
	
instance Ord Square where
	(S a b) <= (S c d) = a < c || (a == c && b <= d)

data Piece = Empty | P Player PieceType

isKing :: Piece -> Bool
isKing (P _ King) = True
isKing _ = False

isRed :: Piece -> Bool
isRed (P Red _) = True
isRed _ = False

isBlack :: Piece -> Bool
isBlack (P Black _) = True
isBlack _ = False

instance Show Piece where
	show Empty = "  "
	show (P color pieceType) = show color ++ show pieceType

data Player = Red | Black
	deriving (Eq)

instance Show Player where
	show Red = "R"
	show Black = "B"

data PieceType = Reg | King

instance Show PieceType where
	show Reg = " "
	show King = "K"

data Move = Push Square Square | Jump Square Square

class Board b where
	s ::  b -> Square -> Piece
	sSet :: b -> Square -> Piece -> b
	legalMoves :: b -> Player -> [Move]
	move :: b -> Move -> b
	move = doMove

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
	sSet = mBSSet
	legalMoves = mBLegalMoves
	
startingBoard :: MapBoard
startingBoard = MapB $ fromList $ L.map startingSquare legalPositions
	where legalPositions = [(x, y) | x <- [1..8], y <- [1..8],
		(mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]
	
startingSquare :: (Int, Int) -> (Square, Piece)
startingSquare (row, col) | row <= 3 = (S row col, P Red Reg)
						  | row >= 6 = (S row col, P Black Reg)
						  | otherwise = (S row col, Empty)
	
mBoardS :: MapBoard -> Square -> Piece
mBoardS (MapB m) s = case M.lookup s m of
	Just piece -> piece
	Nothing -> Empty
	
mBSSet :: MapBoard -> Square -> Piece -> MapBoard
mBSSet (MapB m) s p = MapB $ M.insert s p $ M.delete s m
	

mBLegalMoves :: MapBoard -> Player -> [Move]
mBLegalMoves board player = [Push (S 1 2) (S 2 3)]

validStartingSquares :: MapBoard -> Player -> [Square]
validStartingSquares (MapB m) p = L.map fst (L.filter (matchesPlayer p) (toList m))

matchesPlayer :: Player -> (Square, Piece) -> Bool
matchesPlayer p (_, P player _) = player == p

legalMovesFromSquare :: MapBoard -> Square -> [Move]
legalMovesFromSquare b sq | (length legalJumps) > 0 = legalJumps
						  | otherwise = legalPushes
	where
		legalJumps = jumpsFrom b sq
		legalPushes = pushesFrom b sq
		
jumpsFrom :: MapBoard -> Square -> [Move]
jumpsFrom b s = []

pushesFrom :: MapBoard -> Square -> [Move]
pushesFrom b s = []

doMove :: (Board b) => b -> Move -> b
doMove b (Push s1 s2) = push b s1 s2
doMove b (Jump s1 s2) = jump b s1 s2

push :: (Board b) => b -> Square -> Square -> b
push b s1 s2 = sSet (sSet b s1 Empty) s2 movingPiece
	where
		movingPiece = destPiece (s b s1) s2

jump :: (Board b) => b -> Square -> Square -> b
jump b s1 s2 = sSet (sSet (sSet b s1 Empty) jumped Empty) s2 jumper
	where
		jumped = S ((row s1) + (quot ((row s2) - (row s1)) 2))
				   ((col s2) - (quot ((col s2) - (col s1)) 2))
		jumper = destPiece (s b s1) s2
		
destPiece :: Piece -> Square -> Piece
destPiece p sq = case reachedOppositeEdge p sq of
	True -> if (isRed p) then (P Red King) else (P Black King)
	False -> p
	
reachedOppositeEdge :: Piece -> Square -> Bool
reachedOppositeEdge p sq = if ((isRed p) && (row sq) == 8) || ((isBlack p) && (row sq) == 1)
	then True
	else False

promote :: (Board b) => b -> Square -> b
promote b sq = case s b sq of
	(P Red Reg) -> sSet b sq (P Red King)
	(P Black Reg) -> sSet b sq (P Black King)
	_ -> b