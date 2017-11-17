{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module GameLogic where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import Control.Applicative (Alternative(..),liftA3)
import Control.Monad ()

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import qualified Control.Monad.State as S

-------------------------------------------------------------------------

-- [QUESTION] Is this return type allowed?

main :: ChessBoard ()
main = playGame initialGame

-------------------------------------------------------------------------

-- [QUESTION] Do we need a separate module for Pieces / how would you recommend we refactor this?

data Location = Loc Int Int deriving (Eq, Ord, Show)
data Player = White | Black deriving (Eq, Show)
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
data Piece = P Player PieceType
type Board = Map Location (Maybe Piece) 
-- [QUESTION] Where should we use Game?  Where does the example use Game?
data Game = Game { board :: Board, current :: Player }
data End = Win Player | Tie
 
-- [QUESTION] What should the second argument to State be in the below type?
-- Using Location as of now.

-- [QUESTION] How do we propagate the error with this? use a MaybeT transformer?

type ChessBoard = StateT Board IO
-- 

initialGame :: Game
initialGame = undefined

getNextMove :: String
getNextMove = undefined

-- White to move:
-- E4 E5
-- print board
-- Black to move:

-- takes in a string of format LetterNumber and returns the corresponding Location
inputToLocation :: String -> Maybe (Location, Location)
inputToLocation (x:y:[]) = undefined --if the string is 2 characters then convert it appropriately
inputToLocation _ = Nothing


-- takes in a string of format "E4 E5" and if this is a valid action:
-- updates board with movePiece
-- checks for win
-- prints board
-- sets turn to next player
-- else: 
-- ask for correct input
playGame :: Game -> ChessBoard ()
playGame game = do
    printBoard (board game)
    case (checkForWin (board game)) of 
        (Just (Win White)) -> S.liftIO $ putStrLn "White wins"
        (Just (Win Black)) -> S.liftIO $ putStrLn "Black wins"
        (Just Tie) -> S.liftIO $ putStrLn "Tie. Game Over."
        Nothing -> do
            S.liftIO $ putStr $ "Player " ++ (show (current game)) ++ "'s turn"
            case (inputToLocation (getNextMove)) of 
                Nothing -> S.liftIO $ putStrLn "Incorrect input"
                (Just (from@(Loc x1 y1), to@(Loc x2 y2))) -> do
                        initialBoard <-  S.get
                        case (getPiece initialBoard from) of 
                            Nothing -> S.liftIO $ putStrLn "Incorrect input"
                            (Just pc@(P pl _)) -> if ((validMove pc from to) && (pl == current game))
                                then do
                                    case (handleTurn game from to) of
                                        (Just game') -> playGame game'
                                        Nothing -> playGame game
                                else S.liftIO $ putStrLn "Incorrect input"

-- makes the actual move
-- sets the current player after a move successfully completes
-- [QUESTION] how to check if the move was successfully made?
handleTurn :: Game -> Location -> Location -> Maybe Game
handleTurn game from to = undefined
                        --if to' == to
                        --    then if (current game) == White
                        --            then (Just (Game newBoard Black))
                        --            else (Just (Game newBoard White))
                        --    else Nothing
                        --    where
                        --    (to', newBoard) = case (S.runStateT makeMove (board game)) of 
                        --                        (IO (a, b)) -> (a, b)

                        --    makeMove :: ChessBoard Location
                        --    makeMove =  do
                        --                to' <- movePiece from to
                        --                return to'


-- PrettyPrint our board
printBoard :: Board -> ChessBoard ()
printBoard = undefined

-- look for checkmate cases / tie cases
-- if there are none, return Nothing
-- otherwise return Just Win White / Just Win Black / Just Tie
checkForWin :: Board -> Maybe End
checkForWin = undefined

-- Takes in a Location and uses getPiece to get the piece at the Location
-- updates it
-- If there is a piece at the target Location then capture it 
-- TODO - Handle special scenarios like Castling, en passant etc
movePiece :: Location -> Location -> ChessBoard Location
movePiece from to | from == to = return to
movePiece from to | otherwise  =    do
                                    initialBoard <-  S.get
                                    case (getPiece initialBoard from) of 
                                        Nothing -> return from 
                                        (Just pc) -> do
                                                        toImm <- movePieceOnce from (getImmNextLocation initialBoard pc from to)
                                                        movePiece toImm to
-- if there is a problem, it might loop infinitely because the error is not propagated up

movePieceOnce :: Location -> Location -> ChessBoard Location
movePieceOnce from to = do
                        initialBoard <- S.get
                        case (getPiece initialBoard from) of
                            Nothing -> return from
                            (Just (P colorFrom pcFrom)) -> case (getPiece initialBoard to) of
                                                            Nothing -> S.put (Map.insert from Nothing initialBoard) >>
                                                                    S.put (Map.insert to (Just (P colorFrom pcFrom)) initialBoard) >>
                                                                    return to
                                                            (Just (P colorTo pcTo)) -> if (colorFrom == colorTo)
                                                                then return from
                                                                else S.put (Map.insert from Nothing initialBoard) >>
                                                                    S.put (Map.insert to (Just (P colorFrom pcFrom)) initialBoard) >>
                                                                    return to                                    

getImmNextLocation :: Board -> Piece -> Location -> Location -> Location
getImmNextLocation bd (P _ Bishop) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
                                                                where x' = if x2 > x1
                                                                            then x1 + 1
                                                                            else x1 - 1
                                                                      y' = if y2 > y1
                                                                            then y1 + 1
                                                                            else y1 - 1

getImmNextLocation bd (P _ Rook) l1@(Loc x1 y1) l2@(Loc x2 y2) =  Loc x' y'
                                                                where x' = if x2 > x1
                                                                            then x1 + 1
                                                                            else if x2 == x1
                                                                                then x1
                                                                                else x1 - 1
                                                                      y' = if y2 > y1
                                                                            then y1 + 1
                                                                            else if y2 == y1
                                                                                then y1
                                                                                else y1 - 1                                                                                
getImmNextLocation bd (P _ King) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
getImmNextLocation bd (P _ Knight) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
getImmNextLocation bd (P _ Pawn) l1@(Loc x1 y1) l2@(Loc x2 y2) =  if x1 /= x2
                                                                    then l2
                                                                    else if (abs (y2 - y1)) == 1
                                                                        then l2
                                                                        else Loc x1 (y1 + (y2-y1))
getImmNextLocation bd (P _ Queen) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
                                                                where x' = if x2 > x1
                                                                            then x1 + 1
                                                                            else if x2 == x1
                                                                                then x1
                                                                                else x1 - 1
                                                                      y' = if y2 > y1
                                                                            then y1 + 1
                                                                            else if y2 == y1
                                                                                then y1
                                                                                else y1 - 1                                                                        

-- Takes in a Piece and a proposed Location and returns 
-- whether or not the Piece can move to that Location 
-- according to the rules for each piece
validMove :: Piece -> Location -> Location -> Bool
validMove (P _ King) (Loc x1 y1) (Loc x2 y2) = (dist <= 2 && dist > 0) 
                                    where dist = (x1 - x2)^2 + (y1 - y2)^2
validMove (P _ Queen) (Loc x1 y1) (Loc x2 y2) = validMove (P White Bishop) (Loc x1 y1) (Loc x2 y2) ||
                                                validMove (P White Rook) (Loc x1 y1) (Loc x2 y2)
validMove (P _ Bishop) (Loc x1 y1) (Loc x2 y2) = ((abs (x1 - x2)) == (abs (y1 - y2))) && (abs (x1 - x2)) > 0
validMove (P _ Knight) (Loc x1 y1) (Loc x2 y2) =    (((abs (x1 - x2)) == 2) && ((abs (y1 - y2)) == 1)) ||
                                                    (((abs (y1 - y2)) == 2) && ((abs (x1 - x2)) == 1))
validMove (P _ Rook) (Loc x1 y1) (Loc x2 y2) = ((x1 == x2) && (abs (y1 - y2)) > 0) 
                                            || ((y1 == y2) && (abs (x1 - x2)) > 0)
validMove (P White Pawn) (Loc x1 y1) (Loc x2 y2) = ((x1 == x2) && ((y2 - y1 == 1) || ((y1 == 2) && (y2 - y1 == 2)))) ||
                                                    (((abs (x1 - x2)) == 1) && (y2 - y1 == 1))
validMove (P Black Pawn) (Loc x1 y1) (Loc x2 y2) = ((x1 == x2) && ((y2 - y1 == -1) || ((y1 == 7) && (y2 - y1 == -2)))) ||
                                                    (((abs (x1 - x2)) == 1) && (y2 - y1 == -1))                                                    

-- just gets a Piece from the Location
-- [QUESTION] Do we need the ChessBoard here? 
getPiece :: Board -> Location -> (Maybe Piece)
getPiece mp from = do
                    p <- Map.lookup from mp
                    p

