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

playList :: IO ()
playList = playGameFromList initialGame firstGame

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
                                    if (pieceCanDefendKing game) then putStrLn "queen can defend king"
                                    else putStrLn "other case"
                                    continuePlay game
                    Playing   -> continuePlay game

                where

                continuePlay :: Game -> IO ()
                continuePlay game = do
                    putStr $ (show (current game)) ++ "'s turn: "
                    input <- getLine
                    if input == "exit" then return () else 
                        if input == "printlog" then (printLog game) else do
                            case (getNextMove input) of
                                Nothing -> do
                                            putStrLn "Invalid input"
                                            playGame game
                                (Just move) -> case (runStateT (handleTurn move) game) of
                                                Left s -> do
                                                            putStrLn $ "Uh-oh: " ++ s
                                                            playGame game
                                                Right (_, game') -> playGame game'

playGameFromList:: Game -> [String] -> IO ()
playGameFromList game [] = playGame game
playGameFromList game (input:xs) = do
                                printBoard (board game)
                                case (checkGameStatus game) of 
                                    WhiteWins -> putStrLn "White won."
                                    BlackWins -> putStrLn "Black won."
                                    Tie       -> putStrLn "Game tied."
                                    Checked   -> do
                                                    putStrLn "Check!"
                                                    if (pieceCanDefendKing game) then putStrLn "queen can defend king"
                                                    else putStrLn "other case"
                                                    continuePlay game
                                    Playing   -> continuePlay game

                                where

                                continuePlay :: Game -> IO ()
                                continuePlay game = do
                                    putStrLn $ (show (current game)) ++ "'s turn: "
                                    if input == "exit" then return () else 
                                        if input == "printlog" then (printLog game) else do
                                            case (getNextMove input) of
                                                Nothing -> do
                                                            putStrLn "Invalid input"
                                                            playGameFromList game (input:xs)
                                                (Just move) -> case (runStateT (handleTurn move) game) of
                                                                Left s -> do
                                                                            putStrLn $ "Uh-oh: " ++ s
                                                                            playGame game
                                                                Right (_, game') -> playGameFromList game' xs
                                                
firstGame :: [String]
firstGame = ["E2 E4", "E7 E5",
             "G1 F3", "F7 F6",
             "F3 E5", "F6 E5",
             "D1 H5", "E8 E7",
             "H5 E5", "E7 F7",
             "F1 C4", "D7 D5",
             "C4 D5", "F7 G6",
             "H2 H4", "H7 H5",
             "D5 B7", "C8 B7",
             "E5 F5", "G6 H6",
             "D2 D4", "G7 G5",
             "F5 F7", "D8 E7",
             "H4 G5", "E7 G5",
             "H1 H5"]
             
             
-------------------------------------------------------------------------

printLog :: Game -> IO ()
printLog game = mapM_ putStrLn (map show (moveLog game))

-- this throws exceptions if not a digit - handle that
getNextMove :: String -> Maybe Move
getNextMove (a:m:x:b:n:xs) = if (wr a') && (wr b') &&
                                (wr m') && (wr n') &&
                                (x == ',' || isSpace x)
                             then (Just (Move (Loc a' m') (Loc b' n')))
                             else Nothing
                             where
                             a' = (ord $ toLower a) - 96
                             b' = (ord $ toLower b) - 96
                             m' = digitToInt m
                             n' = digitToInt n

                             wr :: Int -> Bool
                             wr i = i > 0 && i < 9
getNextMove _ = Nothing


-------------------------------------------------------------------------

-- try to do this with foldm / fold

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
                     Nothing -> putStr " x "
                     (Just p) -> putStr (pieceToStr p)

pieceToStr :: Piece -> String
pieceToStr (P Black King) = "BK "
pieceToStr (P Black Queen) = "BQ "
pieceToStr (P Black Bishop) = "BB "
pieceToStr (P Black Knight) = "BN "
pieceToStr (P Black Rook) = "BR "
pieceToStr (P Black Pawn) = "BP "
pieceToStr (P White King) = "WK "
pieceToStr (P White Queen) = "WQ "
pieceToStr (P White Bishop) = "WB "
pieceToStr (P White Knight) = "WN "
pieceToStr (P White Rook) = "WR "
pieceToStr (P White Pawn) = "WP "

-- PrettyPrint our board
printBoard :: Board -> IO ()
printBoard board = do 
                     pBoardX board 8
                     putStrLn "   A   B   C   D   E   F   G   H "
 

-------------------------------------------------------------------------
