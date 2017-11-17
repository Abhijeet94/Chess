{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module GameLogic where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import Control.Applicative (Alternative(..),liftA3)
import Control.Monad ()

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import qualified Control.Monad.State as State

-------------------------------------------------------------------------

main :: IO ()
main = return ()

-------------------------------------------------------------------------

-- [QUESTION] Do we need a separate module for Pieces / how would you recommend we refactor this?

data Location = Loc Int Int deriving (Eq, Ord, Show)
data Player = White | Black
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
data Piece = P Player PieceType
type Board = Map Location (Maybe Piece)
-- [QUESTION] Where should we use Game?  Where does the example use Game?
data Game = Game { board :: Board, current :: Player }
data End = Win Player | Tie
 
-- [QUESTION] What should the second argument to State be in the below type?
-- Using Location as of now.

type ChessBoard = StateT Board IO
-- 

initialGame :: Game
initialGame = undefined

-- White to move:
-- E4 E5
-- print board
-- Black to move:

-- takes in a string of format LetterNumber and returns the corresponding Location
inputToLocation :: String -> Maybe Location
inputToLocation (x:y:[]) = undefined --if the string is 2 characters then convert it appropriately
inputToLocation _ = Nothing

-- takes in a string of format "E4 E5" and if this is a valid action:
-- updates board with movePiece
-- checks for win
-- prints board
-- sets turn to next player
-- else: 
-- ask for correct input
-- [QUESTION] How do we structure this?
handleTurn :: String -> IO()
handleTurn = undefined

-- PrettyPrint our board
printBoard :: ChessBoard Location -> IO()
printBoard = undefined

-- look for checkmate cases / tie cases
-- if there are none, return Nothing
-- otherwise return Just Win White / Just Win Black / Just Tie
checkForWin :: ChessBoard Location -> Maybe End
checkForWin = undefined

-- Takes in a Location and uses getPiece to get the piece at the Location
-- Then takes in a target Location and if the move is valid (use checkMove), updates it
-- If there is a piece at the target Location then capture it 
-- at the end, movePiece should set the turn to the next player
movePiece :: Location -> Location -> ChessBoard Location
movePiece from to = undefined

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
getPiece :: ChessBoard Location -> Location -> Maybe Piece
getPiece cb from = undefined
