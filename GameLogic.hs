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

data Location = Loc Int Int deriving (Eq, Ord, Show)
data Player = White | Black deriving (Eq, Show)
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq, Show)
data Piece = P Player PieceType deriving (Eq, Show)
type Board = Map Location Piece
data Move = Move {src :: Location, dest :: Location}
data Game = Game { board :: Board, current :: Player }
data End = Win Player | Tie
type ChessBoard = StateT Game (Either String)

------------------------------------------------------------------------- 

initialGame :: Game
initialGame = undefined

-------------------------------------------------------------------------

-- throws error inside the state monad ChessBoard
throwError s = S.lift $ Left s

-------------------------------------------------------------------------

-- gets a Piece from the Location, error is thrown if no piece is found
getPiece :: Location -> ChessBoard Piece
getPiece loc = do
                game <- S.get
                case (Map.lookup loc (board game)) of
                    Nothing -> throwError $ "No piece found at location" ++ show loc
                    (Just x) -> return x

-------------------------------------------------------------------------

handleTurn :: Move -> ChessBoard ()
handleTurn move = do
                    (P cl pt) <- getPiece (src move)
                    game <- S.get
                    if (current game) == cl
                        then movePiece move
                        else throwError $ "It is " ++ (show $ current game) ++ "'s turn"

-------------------------------------------------------------------------

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

-------------------------------------------------------------------------

updateBoard :: Piece -> Move -> ChessBoard Location
updateBoard p move = undefined
                        --do
                        --game <- S.get
                        --S.put $ Game (Map.insert (src move) Nothing (board game)) (current game)
                        --S.put $ Game (Map.insert (dest move) p (board game)) (current game)
                        --return (dest move)

-------------------------------------------------------------------------

movePiece :: Move -> ChessBoard ()
movePiece move = do
                    if (src move) == (dest move)
                        then do
                            game <- S.get
                            S.put $ Game (board game) (otherPlayer (current game))
                        else do
                            nextImmLoc <- getNextImmLocation move
                            nextLoc <- movePieceOnce $ Move (src move) nextImmLoc
                            movePiece $ Move nextLoc (dest move)

-------------------------------------------------------------------------


movePieceOnce :: Move -> ChessBoard Location
movePieceOnce move = do
                        pSrc <- getPiece (src move)
                        game <- S.get
                        case (Map.lookup (dest move) (board game)) of
                            Nothing -> updateBoard pSrc move
                            (Just (P clDest _)) -> if clDest == (current game)
                                                    then throwError $ "Invalid move"
                                                    else updateBoard pSrc move

-------------------------------------------------------------------------

getNextImmLocation :: Move -> ChessBoard Location
getNextImmLocation move = do 
                            pSrc <- getPiece (src move)
                            return $ getImmNextLocation pSrc (src move) (dest move) 

getImmNextLocation :: Piece -> Location -> Location -> Location
getImmNextLocation (P _ Bishop) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
                                                                where x' = if x2 > x1
                                                                            then x1 + 1
                                                                            else x1 - 1
                                                                      y' = if y2 > y1
                                                                            then y1 + 1
                                                                            else y1 - 1

getImmNextLocation (P _ Rook) l1@(Loc x1 y1) l2@(Loc x2 y2) =  Loc x' y'
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
getImmNextLocation (P _ King) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
getImmNextLocation (P _ Knight) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
getImmNextLocation (P _ Pawn) l1@(Loc x1 y1) l2@(Loc x2 y2) =  if x1 /= x2
                                                                    then l2
                                                                    else if (abs (y2 - y1)) == 1
                                                                        then l2
                                                                        else Loc x1 (y1 + (y2-y1))
getImmNextLocation (P _ Queen) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
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




-------------------------------------------------------------------------











----------------------------------------------------
----------------------------------------------------
----------------------OLD---------------------------
----------------------------------------------------
----------------------------------------------------





---- just gets a Piece from the Location
--getPiece :: Location -> ChessBoard Piece
--getPiece mp from = do
--                    p <- Map.lookup from mp
--                    p


---- makes the actual move
---- sets the current player after a move successfully completes
---- [QUESTION] how to check if the move was successfully made?
--handleTurn :: Move -> ChessBoard Game
--handleTurn game from to = undefined
--                        --if to' == to
--                        --    then if (current game) == White
--                        --            then (Just (Game newBoard Black))
--                        --            else (Just (Game newBoard White))
--                        --    else Nothing
--                        --    where
--                        --    (to', newBoard) = case (S.runStateT makeMove (board game)) of 
--                        --                        (IO (a, b)) -> (a, b)

--                        --    makeMove :: ChessBoard Location
--                        --    makeMove =  do
--                        --                to' <- movePiece from to
--                        --                return to'


---- look for checkmate cases / tie cases
---- if there are none, return Nothing
---- otherwise return Just Win White / Just Win Black / Just Tie
--checkForWin :: Board -> Maybe End
--checkForWin = undefined

---- Takes in a Location and uses getPiece to get the piece at the Location
---- updates it
---- If there is a piece at the target Location then capture it 
---- TODO - Handle special scenarios like Castling, en passant etc
--movePiece :: Location -> Location -> ChessBoard Location
--movePiece from to | from == to = return to
--movePiece from to | otherwise  =    do
--                                    initialBoard <-  S.get
--                                    case (getPiece initialBoard from) of 
--                                        Nothing -> return from 
--                                        (Just pc) -> do
--                                                        toImm <- movePieceOnce from (getImmNextLocation initialBoard pc from to)
--                                                        movePiece toImm to
---- if there is a problem, it might loop infinitely because the error is not propagated up

--movePieceOnce :: Location -> Location -> ChessBoard Location
--movePieceOnce from to = do
--                        initialBoard <- S.get
--                        case (getPiece initialBoard from) of
--                            Nothing -> return from
--                            (Just (P colorFrom pcFrom)) -> case (getPiece initialBoard to) of
--                                                            Nothing -> S.put (Map.insert from Nothing initialBoard) >>
--                                                                    S.put (Map.insert to (Just (P colorFrom pcFrom)) initialBoard) >>
--                                                                    return to
--                                                            (Just (P colorTo pcTo)) -> if (colorFrom == colorTo)
--                                                                then return from
--                                                                else S.put (Map.insert from Nothing initialBoard) >>
--                                                                    S.put (Map.insert to (Just (P colorFrom pcFrom)) initialBoard) >>
--                                                                    return to                                    

--getImmNextLocation :: Board -> Piece -> Location -> Location -> Location
--getImmNextLocation bd (P _ Bishop) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
--                                                                where x' = if x2 > x1
--                                                                            then x1 + 1
--                                                                            else x1 - 1
--                                                                      y' = if y2 > y1
--                                                                            then y1 + 1
--                                                                            else y1 - 1

--getImmNextLocation bd (P _ Rook) l1@(Loc x1 y1) l2@(Loc x2 y2) =  Loc x' y'
--                                                                where x' = if x2 > x1
--                                                                            then x1 + 1
--                                                                            else if x2 == x1
--                                                                                then x1
--                                                                                else x1 - 1
--                                                                      y' = if y2 > y1
--                                                                            then y1 + 1
--                                                                            else if y2 == y1
--                                                                                then y1
--                                                                                else y1 - 1                                                                                
--getImmNextLocation bd (P _ King) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
--getImmNextLocation bd (P _ Knight) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
--getImmNextLocation bd (P _ Pawn) l1@(Loc x1 y1) l2@(Loc x2 y2) =  if x1 /= x2
--                                                                    then l2
--                                                                    else if (abs (y2 - y1)) == 1
--                                                                        then l2
--                                                                        else Loc x1 (y1 + (y2-y1))
--getImmNextLocation bd (P _ Queen) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
--                                                                where x' = if x2 > x1
--                                                                            then x1 + 1
--                                                                            else if x2 == x1
--                                                                                then x1
--                                                                                else x1 - 1
--                                                                      y' = if y2 > y1
--                                                                            then y1 + 1
--                                                                            else if y2 == y1
--                                                                                then y1
--                                                                                else y1 - 1                                                                        

---- Takes in a Piece and a proposed Location and returns 
---- whether or not the Piece can move to that Location 
---- according to the rules for each piece
--validMove :: Piece -> Location -> Location -> Bool
--validMove (P _ King) (Loc x1 y1) (Loc x2 y2) = (dist <= 2 && dist > 0) 
--                                    where dist = (x1 - x2)^2 + (y1 - y2)^2
--validMove (P _ Queen) (Loc x1 y1) (Loc x2 y2) = validMove (P White Bishop) (Loc x1 y1) (Loc x2 y2) ||
--                                                validMove (P White Rook) (Loc x1 y1) (Loc x2 y2)
--validMove (P _ Bishop) (Loc x1 y1) (Loc x2 y2) = ((abs (x1 - x2)) == (abs (y1 - y2))) && (abs (x1 - x2)) > 0
--validMove (P _ Knight) (Loc x1 y1) (Loc x2 y2) =    (((abs (x1 - x2)) == 2) && ((abs (y1 - y2)) == 1)) ||
--                                                    (((abs (y1 - y2)) == 2) && ((abs (x1 - x2)) == 1))
--validMove (P _ Rook) (Loc x1 y1) (Loc x2 y2) = ((x1 == x2) && (abs (y1 - y2)) > 0) 
--                                            || ((y1 == y2) && (abs (x1 - x2)) > 0)
--validMove (P White Pawn) (Loc x1 y1) (Loc x2 y2) = ((x1 == x2) && ((y2 - y1 == 1) || ((y1 == 2) && (y2 - y1 == 2)))) ||
--                                                    (((abs (x1 - x2)) == 1) && (y2 - y1 == 1))
--validMove (P Black Pawn) (Loc x1 y1) (Loc x2 y2) = ((x1 == x2) && ((y2 - y1 == -1) || ((y1 == 7) && (y2 - y1 == -2)))) ||
--                                                    (((abs (x1 - x2)) == 1) && (y2 - y1 == -1))                                                    



