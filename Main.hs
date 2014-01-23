module Main(main) where

import AI
import Board
import BoardTests

main :: IO ()
main = playGame

playGame :: IO ()
playGame = humanTurn startingBoard (push (square 0 0) (square 0 0)) Black
			
humanTurn :: MapBoard -> Move -> Player -> IO ()
humanTurn b m p = if (canJumpAgain b m p)
	then doTurn b (jumpsFromLastMove b p m) p
	else doTurn b (legalMoves b (otherPlayer p)) (otherPlayer p)

canJumpAgain :: MapBoard -> Move -> Player -> Bool
canJumpAgain b m p = if isJump m
	then length (jumpsFromLastMove b p m) > 0
	else False
	
doTurn :: MapBoard -> [Move] -> Player -> IO ()
doTurn b moves p = do
	putStrLn (show b ++ "\n" ++ "Enter the number of your move\n")
	putStrLn (showMoves 1 moves)
	moveNum <- getLine
	let selectedMove = (moves !! ((read moveNum) - 1))
	putStrLn ("You selected " ++ show selectedMove ++ "\n")
	let boardAfterMove = move b selectedMove
	case winner boardAfterMove of
		Just winningPlayer -> putStrLn (show winningPlayer ++ " wins\n" ++ show boardAfterMove)
		Nothing -> humanTurn boardAfterMove selectedMove p
					
showMoves :: Int -> [Move] -> String
showMoves _ [] = ""
showMoves moveNum (move:rest) = show moveNum ++ ". " ++ (show move) ++ "\n" ++ showMoves (moveNum+1) rest