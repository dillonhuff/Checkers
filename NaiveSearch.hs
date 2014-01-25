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
	
data MoveSeqTree = RootSeq [MoveSeqTree] | MSTree [Move] [MoveSeqTree]
	deriving (Show)
	
moveSequence :: MoveTree -> [MoveSeqTree]
moveSequence (Root children) = [RootSeq $ concat $ map moveSequence children]
moveSequence (MTree (p, m) children) = if (not (isJump m))
	then [MSTree [m] $ concat $ map moveSequence children]
	else concat $ map (jumpSequence p [m]) children

jumpSequence :: Player -> [Move] -> MoveTree -> [MoveSeqTree]
jumpSequence p moves (MTree (pl, move) children) = if (pl == p) && (isJump move)
	then if (children == [])
		then [MSTree (moves ++ [move]) []]
		else concat $ map (jumpSequence p (moves ++ [move])) children
	else [MSTree moves (concat $ map moveSequence children)]

data MoveTree = Root [MoveTree] | MTree (Player, Move) [MoveTree]
	deriving (Show, Eq)

makeMoveTree :: Int -> MapBoard -> Player -> MoveTree
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