module Main(main) where

import AI
import Board
import BoardTests

main :: IO ()
main = allBoardTests

playGame :: IO ()
playGame = humanTurn startingBoard red
	
humanTurn :: MapBoard -> Player -> IO ()
humanTurn b p = do
	case winner b of 
		Just winningPlayer -> putStrLn (show winningPlayer ++ " wins!\n")
		Nothing -> humanMove b p
			
humanMove :: MapBoard -> Player -> IO ()
humanMove b p = do
	let moves = legalMoves b p
	putStrLn (show b ++ "\n" ++ "Enter the number of your move\n")
	putStrLn (showMoves 1 moves)
	moveNum <- getLine
	let selectedMove = (moves !! ((read moveNum) - 1))
	putStrLn ("You selected " ++ show selectedMove ++ "\n")
	let boardAfterMove = move b selectedMove
	if (canJumpAgain boardAfterMove p selectedMove)
		then humanTurn boardAfterMove p
		else case winner boardAfterMove of
			Just winningPlayer -> putStrLn (show winningPlayer ++ " wins!\n")
			Nothing -> humanTurn (computerTurn boardAfterMove (otherPlayer p)) p
	
					
computerTurn :: (Board b) => b -> Player -> b
computerTurn b p = if (canJumpAgain boardAfterMove p selectedMove)
	then computerTurn boardAfterMove p
	else boardAfterMove
	where
		selectedMove = selectMove FMAI b p
		boardAfterMove = move b selectedMove
	
					
showMoves :: Int -> [Move] -> String
showMoves _ [] = ""
showMoves moveNum (move:rest) = show moveNum ++ ". " ++ (show move) ++ "\n" ++ showMoves (moveNum+1) rest