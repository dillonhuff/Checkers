{-# LANGUAGE RankNTypes #-}

module NaiveSearch() where

import Board

data NaiveSearchAI = NSAI ((Board b) => b -> Player -> Float)

pieceDiff :: (Board b) => b -> Player -> Float
pieceDiff b p = fromIntegral (playerPieces - otherPieces)
	where
		playerPieces = (length (kings b p)) + (length (regularPieces b p))
		otherPieces = (length (kings b (otherPlayer p)))
			+ (length (regularPieces b (otherPlayer p)))
			
data GameTree = GTree Float [GameTree]
	deriving (Show)

makeGameTree :: (Board b) => Int -> (b -> Player -> Float) -> Player -> b -> GameTree
makeGameTree depth evalFunc p b = GTree (evalFunc b p) children
	where
		children = if (depth > 0)
			then map (makeGameTree (depth-1) evalFunc (otherPlayer p)) (possibleNextBoards b p)
			else []

possibleNextBoards :: (Board b) => b -> Player -> [b]
possibleNextBoards b p = concat $ map (moveSequence b p) (legalMoves b p)

moveSequence :: (Board b) => b -> Player -> Move -> [b]
moveSequence b m = if (canJumpAgain b p m)
	then concat $ moveSequence (move 

gTreeToList :: GameTree -> [Float]
gTreeToList (GTree score []) = [score]
gTreeToList (GTree score children) = [score] ++ concat (map gTreeToList children)