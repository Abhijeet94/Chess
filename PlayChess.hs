{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module PlayChess where
import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import qualified Control.Monad.State as S    
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
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
                case (checkGameStatus game) of 
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
                    putStr $ (show (current game)) ++ "'s turn: "
                    input <- getLine
                    if input == "exit" then return () else do
                    case (getNextMove input) of
                        Nothing -> do
                                    putStrLn "Invalid input"
                                    playGame game
                        (Just move) -> case (runStateT (handleTurn move) game) of
                                        Left s -> do
                                                    putStrLn $ "Uh-oh: " ++ s
                                                    playGame game
                                        Right (_, game') -> playGame game'


-------------------------------------------------------------------------

-- this throws exceptions if not a digit - handle that
getNextMove :: String -> Maybe Move
getNextMove (a:m:x:b:n:xs) = if (wr a') && (wr b') &&
                                (wr m') && (wr n') &&
                                (x == ',' || isSpace x)
                             then (Just (Move (Loc a' m') (Loc b' n')))
                             else Nothing
                             where
                             a' = (digitToInt $ toLower a) - 9
                             b' = (digitToInt $ toLower b) - 9
                             m' = digitToInt m
                             n' = digitToInt n

                             wr :: Int -> Bool
                             wr i = i > 0 && i < 9
getNextMove _ = Nothing


-------------------------------------------------------------------------

pBoardX :: Board -> Int -> IO ()
pBoardX b 1 = do
               putStr "1 "
               pBoardXY b 1 1
pBoardX b x = do
               putStr ((show x)++" ")
               pBoardXY b x 1
               pBoardX b (x-1)

pBoardXY :: Board -> Int -> Int -> IO ()
pBoardXY b y 8 = do
                   printPiece b 8 y
                   putStrLn ""
pBoardXY b y x = do 
                   printPiece b x y
                   putStr " "
                   pBoardXY b y (x+1)

printPiece :: Board -> Int -> Int -> IO ()
printPiece b x y = case (Map.lookup (Loc x y) b) of 
                     Nothing -> putStr "x "
                     (Just p) -> putStr (pieceToStr p)

pieceToStr :: Piece -> String
pieceToStr (P _ King) = "K "
pieceToStr (P _ Queen) = "Q "
pieceToStr (P _ Bishop) = "B "
pieceToStr (P _ Knight) = "k "
pieceToStr (P _ Rook) = "R "
pieceToStr (P _ Pawn) = "P "

-- PrettyPrint our board
printBoard :: Board -> IO ()
printBoard board = do 
                     pBoardX board 8
                     putStrLn "  A  B  C  D  E  F  G  H "
 

-------------------------------------------------------------------------
