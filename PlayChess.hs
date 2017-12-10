{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module PlayChess where
import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import qualified Control.Monad.State as S    
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import GameLogic
import TransC as T

-------------------------------------------------------------------------

main :: IO ()
main = play

play :: IO ()
play = playGame initialGame True

playList :: IO ()
playList = playGameFromList initialGame ["B1 C3","G8 F6","C3 B5","F6 G4","B5 D6","G4 E3"]

-------------------------------------------------------------------------

printb :: Output m => String -> Bool -> m ()
printb s b = if b then T.write s else T.write ""

playGame :: (Input m, Output m) => Game -> Bool -> m ()
playGame game doPrint = do
                        if doPrint then printBoard (board game)
                        else printb "" doPrint
                        case (checkGameStatus game) of 
                            WhiteWins -> printb "White won.\n" doPrint
                            BlackWins -> printb "Black won.\n" doPrint
                            Tie       -> printb "Game tied.\n" doPrint
                            Checked   -> do
                                            printb "Check!\n" doPrint
                                            --if (pieceCanDefendKing game) then T.write "king can defend itself\n"
                                            --else print "other case\n"
                                            continuePlay game
                            Playing   -> continuePlay game

                        where

                        continuePlay :: (Input m, Output m) => Game -> m ()
                        continuePlay game = do
                            printb ((show (current game)) ++ "'s turn: \n") doPrint
                            ms <- T.input
                            case ms of
                                Nothing -> playGame game False
                                (Just input) -> 
                                    if input == "exit" then return () else 
                                        if input == "printlog" then (printLog game) else do
                                            case (getNextMove input) of
                                                Nothing -> do
                                                            printb "Invalid input\n" True
                                                            playGame game False
                                                (Just move) -> case (runStateT (handleTurn move) game) of
                                                                Left s -> do
                                                                            printb ("Uh-oh: " ++ s ++ "\n") True
                                                                            playGame game False
                                                                Right (_, game') -> playGame game' True
                

playGameFromList :: (Input m, Output m) => Game -> [String] -> m ()
playGameFromList game [] = playGame game True
playGameFromList game (input:xs) = do
                                    case (checkGameStatus game) of 
                                        WhiteWins -> T.write ""
                                        BlackWins -> T.write ""
                                        Tie       -> T.write ""
                                        Checked   -> do
                                                        T.write ""
                                                        if (pieceCanDefendKing game) then T.write ""
                                                        else T.write ""
                                                        continuePlay game
                                        Playing   -> continuePlay game

                                    where

                                    continuePlay :: (Input m, Output m) => Game -> m ()
                                    continuePlay game = do
                                                        T.write ""
                                                        if input == "exit" then return () else 
                                                            if input == "printlog" then (printLog game) else do
                                                                case (getNextMove input) of
                                                                    Nothing -> do
                                                                                T.write ""
                                                                                playGameFromList game (input:xs)
                                                                    (Just move) -> case (runStateT (handleTurn move) game) of
                                                                                    Left s -> do
                                                                                                T.write ""
                                                                                                playGame game True
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

printLog :: Output m => Game -> m ()
printLog game = mapM_ T.write (map show (moveLog game))

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

pBoardX :: Output m => Board -> Int -> m ()
pBoardX b 1 = do
               T.write "1 "
               pBoardXY b 1 1
pBoardX b x = do
               T.write ((show x)++" ")
               pBoardXY b x 1
               pBoardX b (x-1)

pBoardXY :: Output m => Board -> Int -> Int -> m ()
pBoardXY b y 8 = do
                   printPiece b 8 y
                   T.write "\n"
pBoardXY b y x = do 
                   printPiece b x y
                   T.write " "
                   pBoardXY b y (x+1)

printPiece :: Output m => Board -> Int -> Int -> m ()
printPiece b x y = case (Map.lookup (Loc x y) b) of 
                     Nothing -> T.write " x "
                     (Just p) -> T.write (pieceToStr p)

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
printBoard :: Output m => Board -> m ()
printBoard board = do 
                     pBoardX board 8
                     T.write "   A   B   C   D   E   F   G   H \n"
 

-------------------------------------------------------------------------
