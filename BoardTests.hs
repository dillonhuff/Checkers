module BoardTests(main) where

import Test.QuickCheck
import Board

main = do
	quickCheck prop_switchesPlayer_changesPlayer
	quickCheck prop_switchesPlayer_twiceGoesBackToOriginal
	
instance Arbitrary Player where
	arbitrary = elements [red, black]

prop_switchesPlayer_changesPlayer p = otherPlayer p /= p

prop_switchesPlayer_twiceGoesBackToOriginal p = otherPlayer (otherPlayer p) == p

instance Arbitrary PieceType where
	arbitrary = elements [regular, king]
	
instance Arbitrary Square where
	arbitrary = do
		row <- arbitrary
		col <- arbitrary
		return $ square row col
		
instance Arbitrary Piece where
	arbitrary = do
		n <- choose (0, 2) :: Gen Int
		player <- arbitrary
		pieceType <- arbitrary
		return $ case n of
			0 -> emptyPiece
			_ -> piece player pieceType
			
instance Arbitrary Move where
	arbitrary = do
		s1 <- arbitrary
		s2 <- arbitrary
		elements [push s1 s2, jump s1 s2]
	
