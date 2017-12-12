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
play = playGame initialGame 

-------------------------------------------------------------------------

-- Main entry point of the command line interface to Chess game.
-- Drives all the IO and uses the GameLogic functions to play

playGame :: (Input m, Output m) => Game -> m ()
playGame game = do
  printBoard (board game)
  case (checkGameStatus game) of 
      WhiteWins -> T.write "White won.\n" 
      BlackWins -> T.write "Black won.\n" 
      Tie       -> T.write "Game tied.\n" 
      Checked   -> do
                      T.write "Check!\n" 
                      continuePlay game
      Playing   -> continuePlay game

  where

  continuePlay :: (Input m, Output m) => Game -> m ()
  continuePlay game = do
      T.write ((show (current game)) ++ "'s turn: \n") 
      ms <- T.input
      case ms of
          Nothing -> return()
          (Just input) -> 
              if input == "exit" then return () else 
              if input == "printlog" then (printLog game) else do
              case (getNextMove input) of
                  Nothing -> do
                              T.write "Invalid input\n" 
                              playGame game 
                  (Just move) -> case (runStateT (handleTurn move) game) of
                                  Left "getPawnPromotionPiece" -> 
                                                  handlePawnPromotion game move
                                  Left s -> do
                                              T.write ("Uh-oh: " ++ s ++ "\n") 
                                              playGame game 
                                  Right (_, game') -> playGame game' 

  handlePawnPromotion game move = do
      T.write "Enter piece for pawn promotion: \n" 
      inputPc <- T.input
      case inputPc of
        Nothing -> return ()
        (Just pawnPromPcInput) -> case (toPiece pawnPromPcInput) of
                      Nothing -> do
                        T.write "Invalid Piece\n"
                        playGame game
                      pc -> do
                              let game2 = setChessBoardPiece pc game
                              case (runStateT (handleTurn move) game2) of
                                Left s -> do
                                  T.write ("Uh-oh: " ++ s ++ "\n") 
                                  playGame game 
                                Right (_, game') -> do
                                  let game2' = setChessBoardPiece Nothing game'
                                  playGame game2' 

  setChessBoardPiece :: (Maybe PieceType) -> Game -> Game
  setChessBoardPiece pc game = 
                            case (runStateT (setPromotedPieceInGame pc) game) of
                                Left s -> game
                                Right (_, game') -> game'
             
-------------------------------------------------------------------------

printLog :: Output m => Game -> m ()
printLog game = mapM_ T.write (map show (moveLog game))

getNextMove :: String -> Maybe Move
getNextMove (a:m:x:b:n:xs) = if validChars a m b n &&
                                (wr a') && (wr b') &&
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

                             validChars :: Char -> Char -> Char -> Char -> Bool
                             validChars a m b n = ((a >= 'a' && a <= 'h') || 
                                                   (a >= 'A' && a <= 'H')) && 
                                                  ((b >= 'a' && b <= 'h') || 
                                                   (b >= 'A' && b <= 'H')) &&
                                                  (m >= '1' && m <= '8') && 
                                                  (n >= '1' && n <= '8')

getNextMove _ = Nothing

toPiece :: String -> (Maybe PieceType)
toPiece "Queen" = Just Queen
toPiece "Bishop" = Just Bishop
toPiece "Knight" = Just Knight
toPiece "Rook" = Just Rook
toPiece "Pawn" = Just Pawn
toPiece _ = Nothing
-------------------------------------------------------------------------

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
