{-# LANGUAGE RankNTypes #-}

module NaiveSearch(
	NaiveSearchAI(NSAI),
	pieceDiff) where

import Data.List
import AI
import Board


data NaiveSearchAI = NSAI ((Board b) => b -> Float)

instance AI NaiveSearchAI where
	selectMove = selectMoveNSAI

pieceDiff :: (Board b) => Player -> b -> Float
pieceDiff p b = fromIntegral (playerPieces - otherPieces)
	where
		playerPieces = (length (kings b p)) + (length (regularPieces b p))
		otherPieces = (length (kings b (otherPlayer p)))
			+ (length (regularPieces b (otherPlayer p)))
			
selectMoveNSAI :: (Board b) => NaiveSearchAI -> b -> Player -> Move
selectMoveNSAI (NSAI evalFunc) b p = pickMove b p evalFunc (makeMoveTree 3 b p)

pickMove :: (Board b) => b -> Player -> (b -> Float) -> MoveTree -> Move
pickMove b p evalFunc (Root children) = getMove $ fst $ maximumBy bestScore childrenWithScores
	where
		childrenWithScores = zip children (map (moveScore b evalFunc p) children)
		
getMove :: MoveTree -> Move
getMove (MTree (p, m) _) = m
getMove _ = error "No move possible"
		
bestScore :: (MoveTree, Float) -> (MoveTree, Float) -> Ordering
bestScore (_, score1) (_, score2) = if (score1 < score2)
	then LT
	else if (score1 > score2)
		then GT
		else EQ

moveScore :: (Board b) => b -> (b -> Float) -> Player -> MoveTree -> Float
moveScore b evalFunc p (MTree (_, m) []) = evalFunc (move b m)
moveScore b evalFunc p (MTree (pl, m) children) = if p == pl
	then maximum (map (moveScore (move b m) evalFunc p) children)
	else minimum (map (moveScore (move b m) evalFunc p) children)

data MoveTree = Root [MoveTree] | MTree (Player, Move) [MoveTree]
	deriving (Show, Eq)

makeMoveTree :: (Board b) => Int -> b -> Player -> MoveTree
makeMoveTree depth board player = Root $ map (turn depth board player) (legalMoves board player)

turn :: (Board b) => Int -> b -> Player -> Move -> MoveTree
turn depth b p m = if (depth > 0) 
	then if (canJumpAgain b m p)
		then MTree (p, m) $ map (turn (depth-1) boardAfterMove p) (jumpsFromLastMove boardAfterMove p m)
		else MTree (p, m) $ map (turn (depth-1) boardAfterMove nextPlayer) (legalMoves boardAfterMove nextPlayer)
	else MTree (p, m) []
	where
		boardAfterMove = move b m
		nextPlayer = otherPlayer p

canJumpAgain :: (Board b) => b -> Move -> Player -> Bool
canJumpAgain b m p = if isJump m
	then length (jumpsFromLastMove b p m) > 0
	else False
	
moveTreeToList :: MoveTree -> [Move]
moveTreeToList (Root children) = concat $ map moveTreeToList children
moveTreeToList (MTree (p, m) []) = [m]
moveTreeToList (MTree (p, m) children) = m:(concat $ map moveTreeToList children)