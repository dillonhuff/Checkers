module Main(main) where

import Board

main :: IO ()
main = do
	let board = startingBoard
	let moves = legalMoves board Red
	putStrLn (show moves)
	putStrLn (show board ++ "\n" ++ "Enter the number of your move\n")
	moveNum <- getLine
	putStrLn ("You selected " ++ show (moves !! (read moveNum)) ++ "\n")