module BoardTests(allBoardTests) where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Board

allBoardTests :: IO ()
allBoardTests = do
	quickCheck otherPlayer_switchesPlayer
	quickCheck otherPlayer_applyTwiceIsSameAsNotApply
	quickCheck square_correctRow
	quickCheck square_correctCol
	quickCheck move_pushFromIsEmptyAfterMove
	quickCheck move_pushToIsOccupiedAfterMove
	quickCheck move_jumpFromIsEmptyAfterMove
	quickCheck move_jumpToIsOccupiedAfterMove
	quickCheck legalMoves_endOnBoard
	quickCheck legalMoves_dontEndInOccupiedSpaces
	
instance Arbitrary Player where
	arbitrary = elements [Red, Black]
	
otherPlayer_switchesPlayer :: Player -> Bool
otherPlayer_switchesPlayer p = otherPlayer p /= p

otherPlayer_applyTwiceIsSameAsNotApply :: Player -> Bool
otherPlayer_applyTwiceIsSameAsNotApply p = otherPlayer (otherPlayer p) == p

instance Arbitrary PieceType where
	arbitrary = elements [regular, king]
	
instance Arbitrary Square where
	arbitrary = do
		row <- arbitrary
		col <- arbitrary
		return $ square row col

square_correctRow :: Int -> Int -> Bool
square_correctRow r c = row (square r c) == r

square_correctCol :: Int -> Int -> Bool
square_correctCol r c = col (square r c) == c

instance Arbitrary Piece where
	arbitrary = do
		n <- choose (0, 2) :: Gen Int
		player <- arbitrary
		pieceType <- arbitrary
		return $ case n of
			0 -> Empty
			_ -> P player pieceType
			
instance Arbitrary Move where
	arbitrary = do
		s1 <- arbitrary
		s2 <- arbitrary
		elements [push s1 s2, jump s1 s2]
		
instance Arbitrary MapBoard where
	arbitrary = do
		elements <- listOf (arbitrary :: Gen (Square, Piece))
		return $ board (take 32 elements)
		
move_pushFromIsEmptyAfterMove :: MapBoard -> Square -> Square -> Bool
move_pushFromIsEmptyAfterMove b s1 s2 = s (move b (push s1 s2)) s1 == Empty

move_pushToIsOccupiedAfterMove :: MapBoard -> Square -> Square -> Bool
move_pushToIsOccupiedAfterMove b s1 s2 = s (move b (push s1 s2)) s2 == s b s1

move_jumpFromIsEmptyAfterMove :: MapBoard -> Square -> Square -> Bool
move_jumpFromIsEmptyAfterMove b s1 s2 = s (move b (jump s1 s2)) s1 == Empty

move_jumpToIsOccupiedAfterMove :: MapBoard -> Square -> Square -> Bool
move_jumpToIsOccupiedAfterMove b s1 s2 = s (move b (jump s1 s2)) s2 == s b s1

isLegalMove :: (Board b) => b -> Move -> Bool
isLegalMove b m = elem m (legalMoves b Red) || elem m (legalMoves b Black)

legalMoves_endOnBoard :: MapBoard -> Player -> Bool
legalMoves_endOnBoard b p = (filter (==False) $ map (isInBounds b) (map end (legalMoves b p))) == []

legalMoves_dontEndInOccupiedSpaces :: MapBoard -> Player -> Bool
legalMoves_dontEndInOccupiedSpaces b p = filter (\x -> (s b x) /= Empty) (map end (legalMoves b p)) == []

