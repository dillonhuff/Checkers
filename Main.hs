module Main(main) where

import Board

main :: IO ()
main = playGame

playGame :: IO ()
playGame = turn startingBoard Red
	
turn :: MapBoard -> Player -> IO ()
turn b p = do
	let moves = legalMoves b p
	putStrLn (show b ++ "\n" ++ "Enter the number of your move\n")
	putStrLn (show moves)
	moveNum <- getLine
	let selectedMove = (moves !! (read moveNum))
	putStrLn ("You selected " ++ show selectedMove ++ "\n")
	let boardAfterMove = move b selectedMove
	case winner boardAfterMove of
		Just winningPlayer -> putStrLn (show winningPlayer ++ " wins!\n")
		Nothing -> turn boardAfterMove nextPlayer
			where
				nextPlayer = if (p == Red) then Black else Red
	