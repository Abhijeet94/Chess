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

main :: (Input m, Output m) => m ()
main = play

play :: (Input m, Output m) => m ()
play = playGame initialGame True

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
