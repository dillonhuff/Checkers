module Board(
	startingBoard,
	Board(s, legalMoves, move, winner, canJumpAgain, kings, regularPieces),
	Player(Red, Black),
	Square,
	MapBoard,
	Move,
	isJump,
	otherPlayer) where

import Data.List as L
import Data.Map as M

data Square = S Int Int

instance Show Square where
	show (S a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance Eq Square where
	(S a b) == (S c d) = a == c && b == d
	
addS :: Square -> Square -> Square
addS (S a b) (S c d) = S (a+c) (b+d)

subS :: Square -> Square -> Square
subS (S a b) (S c d) = S (a-c) (b-d)

quotS :: Square -> Int -> Square
quotS (S a b) c = S (quot a c) (quot b c)

mulS :: Int -> Square -> Square
mulS c (S a b) = S (c*a) (c*b)
	
row :: Square -> Int
row (S a _) = a

col :: Square -> Int
col (S _ b) = b
	
instance Ord Square where
	(S a b) <= (S c d) = a < c || (a == c && b <= d)

data Piece = Empty | P Player PieceType
	deriving (Eq)

isKing :: Piece -> Bool
isKing (P _ King) = True
isKing _ = False

isRegular :: Piece -> Bool
isRegular (P _ Reg) = True
isRegular _ = False

isRed :: Piece -> Bool
isRed (P Red _) = True
isRed _ = False

isBlack :: Piece -> Bool
isBlack (P Black _) = True
isBlack _ = False

isEmpty :: Piece -> Bool
isEmpty Empty = True
isEmpty _ = False

instance Show Piece where
	show Empty = "  "
	show (P color pieceType) = show color ++ show pieceType

data Player = Red | Black
	deriving (Eq)
	
otherPlayer :: Player -> Player
otherPlayer Red = Black
otherPlayer Black = Red

instance Show Player where
	show Red = "R"
	show Black = "B"

data PieceType = Reg | King
	deriving (Eq)

instance Show PieceType where
	show Reg = " "
	show King = "K"

data Move = Push Square Square | Jump Square Square
	deriving (Show)
	
isJump :: Move -> Bool
isJump (Jump _ _) = True
isJump _ = False

class Board b where
	s ::  b -> Square -> Piece
	sSet :: b -> Square -> Piece -> b
	legalMoves :: b -> Player -> [Move]
	winner :: b -> Maybe Player
	canJumpAgain :: b -> Player -> Move -> Bool
	kings :: b -> Player -> [Square]
	regularPieces :: b -> Player -> [Square]
	move :: b -> Move -> b
	move = doMove

data MapBoard = MapB (Map Square Piece)

instance Show MapBoard where
	show = mBShow
	
mBShow :: MapBoard -> String
mBShow (MapB squaresToPieces) = "  1 2 3 4 5 6 7 8\n" ++ showRows 1 (assocs squaresToPieces)

showRows :: Int -> [(Square, Piece)] -> String
showRows _ [] = ""
showRows rowNum rows = show rowNum ++ " " ++ (showRow $ (take 4 rows)) ++ (showRows (rowNum+1) (drop 4 rows))

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
	winner = mBWinner
	canJumpAgain = mBCanJumpAgain
	kings = mBKings
	regularPieces = mBRegularPieces
	
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
mBLegalMoves b p = if ((length jumps) > 0)
	then jumps
	else possibleMoves
	where
		possibleMoves = concat (L.map (legalMovesFromSquare b) (validStartingSquares b p))
		jumps = L.filter isJump possibleMoves

validStartingSquares :: MapBoard -> Player -> [Square]
validStartingSquares (MapB m) p = L.map fst (L.filter (matchesPlayer p) (toList m))

matchesPlayer :: Player -> (Square, Piece) -> Bool
matchesPlayer p (_, P player _) = player == p
matchesPlayer _ _ = False

legalMovesFromSquare :: MapBoard -> Square -> [Move]
legalMovesFromSquare b sq | (length legalJumps) > 0 = legalJumps
						  | otherwise = legalPushes
	where
		legalJumps = jumpsFrom b sq
		legalPushes = pushesFrom b sq
		
jumpsFrom :: MapBoard -> Square -> [Move]
jumpsFrom b sq = L.filter (isValidJump b) possibleJumps
	where
		possibleJumps = if isKing (s b sq)
			then kingJumps sq
			else regJumps b sq
			
isValidJump :: MapBoard -> Move -> Bool
isValidJump b@(MapB m) (Jump s1 s2) = if (oppositeColors b jumpedSquare s1) 
	&& (isEmpty (s b s2)) && member s2 m
	then True
	else False
	where
		jumpedSquare = addS s1 (quotS (subS s2 s1) 2)
isValidJump _ _ = False

oppositeColors :: (Board b) => b -> Square -> Square -> Bool
oppositeColors b s1 s2 = if (isEmpty (s b s1)) || (isEmpty (s b s2))
	then False
	else if (isRed (s b s1) /= isRed (s b s2))
		then True
		else False

kingJumps :: Square -> [Move]
kingJumps sq = [Jump sq (addS sq (S a b)) | a <- [-2, 2], b <- [-2, 2]]

regJumps :: (Board b) => b -> Square -> [Move]
regJumps b sq = if isRed (s b sq)
	then [Jump sq (addS sq (S a b)) | a <- [2], b <- [-2, 2]]
	else [Jump sq (addS sq (S a b)) | a <- [-2], b <- [-2, 2]]
			
pushesFrom :: MapBoard -> Square -> [Move]
pushesFrom b sq = L.filter (isValidPush b) possiblePushes
	where
		possiblePushes = if isKing (s b sq)
			then kingPushes sq
			else regPushes b sq

isValidPush :: MapBoard -> Move -> Bool
isValidPush b@(MapB m) (Push s1 s2) = isEmpty (s b s2) && member s2 m
isValidPush _ _ = False

kingPushes :: Square -> [Move]
kingPushes sq = [Push sq (addS sq (S a b)) | a <- [-1, 1], b <- [-1, 1]]

regPushes :: (Board b) => b -> Square -> [Move]
regPushes  b sq = if isRed (s b sq)
	then [Push sq (addS sq (S a b)) | a <- [1], b <- [-1, 1]]
	else [Push sq (addS sq (S a b)) | a <- [-1], b <- [-1, 1]]

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
		jumped = addS s1 (quotS (subS s2 s1) 2)
		jumper = destPiece (s b s1) s2
		
destPiece :: Piece -> Square -> Piece
destPiece p sq = case reachedOppositeEdge p sq of
	True -> if (isRed p) then (P Red King) else (P Black King)
	False -> p
	
reachedOppositeEdge :: Piece -> Square -> Bool
reachedOppositeEdge p sq = if ((isRed p) && (row sq) == 8)
	|| ((isBlack p) && (row sq) == 1)
	then True
	else False

promote :: (Board b) => b -> Square -> b
promote b sq = case s b sq of
	(P Red Reg) -> sSet b sq (P Red King)
	(P Black Reg) -> sSet b sq (P Black King)
	_ -> b
	
mBWinner :: MapBoard -> Maybe Player
mBWinner (MapB m) = if ((numPieces Red) == 0)
	then Just Black
	else if ((numPieces Black) == 0)
		then Just Red
		else Nothing
	where
		numPieces p = length $ L.filter (matchesPlayer p) (toList m)
		
mBCanJumpAgain :: MapBoard -> Player -> Move -> Bool
mBCanJumpAgain b p m = length (L.filter (jumpFromLastLanding m) (legalMoves b p)) > 0

jumpFromLastLanding :: Move -> Move -> Bool
jumpFromLastLanding (Jump _ landing) (Jump starting _) = landing == starting
jumpFromLastLanding _ _ = False

mBKings :: MapBoard -> Player -> [Square]
mBKings (MapB m) p = L.map fst (L.filter squareHasKing (L.filter (matchesPlayer p) (toList m)))
	where
		squareHasKing squareAndPiece = isKing $ snd squareAndPiece

mBRegularPieces :: MapBoard -> Player -> [Square]
mBRegularPieces (MapB m) p = L.map fst (L.filter squareHasReg (L.filter (matchesPlayer p) (toList m)))
	where
		squareHasReg squareAndPiece = isRegular $ snd squareAndPiece