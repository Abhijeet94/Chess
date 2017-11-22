{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module PlayChess where
import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import qualified Control.Monad.State as S    
import GameLogic

-------------------------------------------------------------------------

main :: IO ()
main = play

play :: IO ()
play = playGame initialGame

-------------------------------------------------------------------------

playGame :: Game -> IO ()
playGame game = do
                printBoard (board game)
                case (checkForWin (board game)) of 
                    WhiteWins -> putStrLn "White won."
                    BlackWins -> putStrLn "Black won."
                    Tie       -> putStrLn "Game tied."
                    Checked   -> do
                                    putStrLn "Check!"
                                    continuePlay game
                    Playing   -> continuePlay game

                where

                continuePlay :: Game -> IO ()
                continuePlay game = do
                    putStr $ "Player " ++ (show (current game)) ++ "'s turn: "
                    input <- getLine
                    case (getNextMove input) of
                        Nothing -> do
                                    putStrLn "Invalid input"
                                    playGame game
                        (Just move) -> case (runStateT (handleTurn move) game) of
                                        Left s -> do
                                                    putStrLn $ "Uh-oh: " ++ s
                                                    playGame game
                                        Right (_, game') -> do
                                                            printBoard (board game')
                                                            playGame game'




-------------------------------------------------------------------------

getNextMove :: String -> Maybe Move
getNextMove = undefined


-------------------------------------------------------------------------

-- PrettyPrint our board
printBoard :: Board -> IO ()
printBoard = undefined  

-------------------------------------------------------------------------
