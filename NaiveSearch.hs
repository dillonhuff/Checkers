{-# LANGUAGE RankNTypes #-}

module NaiveSearch() where

import Data.List
import Board

data NaiveSearchAI = NSAI ((Board b) => b -> Float)

pieceDiff :: (Board b) => Player -> b -> Float
pieceDiff p b = fromIntegral (playerPieces - otherPieces)
	where
		playerPieces = (length (kings b p)) + (length (regularPieces b p))
		otherPieces = (length (kings b (otherPlayer p)))
			+ (length (regularPieces b (otherPlayer p)))
	
data MoveTree = Root [MoveTree] | MTree Move [MoveTree]
	deriving (Show)

makeMoveTree :: Int -> MapBoard -> Player -> MoveTree
makeMoveTree depth board player = Root $ map (turn depth board player) (legalMoves board player)

turn :: (Board b) => Int -> b -> Player -> Move -> MoveTree
turn depth b p m = if (depth > 0) 
	then if (canJumpAgain b m p)
		then MTree m $ map (turn (depth-1) boardAfterMove p) (jumpsFromLastMove boardAfterMove p m)
		else MTree m $ map (turn (depth-1) boardAfterMove nextPlayer) (legalMoves boardAfterMove nextPlayer)
	else MTree m []
	where
		boardAfterMove = move b m
		nextPlayer = otherPlayer p

canJumpAgain :: (Board b) => b -> Move -> Player -> Bool
canJumpAgain b m p = if isJump m
	then length (jumpsFromLastMove b p m) > 0
	else False