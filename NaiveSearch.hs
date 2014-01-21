{-# LANGUAGE RankNTypes #-}

module NaiveSearch() where

import Board

data NaiveSearchAI = NSAI ((Board b) => b -> Player -> Float)

pieceDiff :: (Board b) => b -> Player -> Float
pieceDiff b p = fromIntegral (playerPieces - otherPieces)
	where
		playerPieces = (length (kings b p)) + (length (regularPieces b p))
		otherPieces = (length (kings b (otherPlayer p))) + (length (regularPieces b (otherPlayer p)))