module BoardTests(main) where

import Test.QuickCheck
import Board

main = do
	quickCheck prop_switchesPlayer_changesPlayer
	quickCheck prop_switchesPlayer_twiceGoesBackToOriginal
	
instance Arbitrary Player where
	arbitrary = elements [Red, Black]

prop_switchesPlayer_changesPlayer p = otherPlayer p /= p

prop_switchesPlayer_twiceGoesBackToOriginal p = otherPlayer (otherPlayer p) == p

