module AI(
	AI(selectMove),
	FirstMoveAI(FMAI)
	) where

import Board

class AI a where
	selectMove :: (Board b) => a -> b -> Player -> Move
	
--Simple AI that picks the first legal move available

data FirstMoveAI = FMAI

instance AI FirstMoveAI where
	selectMove = firstLegalMove
	
firstLegalMove :: (Board b) => FirstMoveAI -> b -> Player -> Move
firstLegalMove ai b p = head (legalMoves b p)