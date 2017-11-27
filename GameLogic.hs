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
data GameStatus = BlackWins | WhiteWins | Checked | Tie | Playing
type ChessBoard = StateT Game (Either String)

------------------------------------------------------------------------- 

initialGame :: Game
initialGame = Game (Map.fromList pos) White
                where
                pos :: [(Location, Piece)]
                pos =  [(Loc 1 1, P White Rook), (Loc 2 1, P White Knight), 
                        (Loc 3 1, P White Bishop), (Loc 4 1, P White Queen),
                        (Loc 5 1, P White King), (Loc 6 1, P White Bishop),
                        (Loc 7 1, P White Knight), (Loc 8 1, P White Rook),
                        (Loc 1 8, P Black Rook), (Loc 2 8, P Black Knight), 
                        (Loc 3 8, P Black Bishop), (Loc 4 8, P Black Queen),
                        (Loc 5 8, P Black King), (Loc 6 8, P Black Bishop),
                        (Loc 7 8, P Black Knight), (Loc 8 8, P Black Rook),
                        (Loc 1 2, P White Pawn), (Loc 2 2, P White Pawn), 
                        (Loc 3 2, P White Pawn), (Loc 4 2, P White Pawn),
                        (Loc 5 2, P White Pawn), (Loc 6 2, P White Pawn),
                        (Loc 7 2, P White Pawn), (Loc 8 2, P White Pawn),
                        (Loc 1 7, P Black Pawn), (Loc 2 7, P Black Pawn), 
                        (Loc 3 7, P Black Pawn), (Loc 4 7, P Black Pawn),
                        (Loc 5 7, P Black Pawn), (Loc 6 7, P Black Pawn),
                        (Loc 7 7, P Black Pawn), (Loc 8 7, P Black Pawn)]

-------------------------------------------------------------------------

-- throws error inside the state monad ChessBoard
throwError s = S.lift $ Left s

-------------------------------------------------------------------------

-- gets a Piece from the Location, error is thrown if no piece is found
getPiece :: Location -> ChessBoard Piece
getPiece loc = do
                game <- S.get
                case (Map.lookup loc (board game)) of
                    Nothing -> throwError $ "No piece found at location " ++ show loc
                    (Just x) -> return x

-------------------------------------------------------------------------

handleTurn :: Move -> ChessBoard ()
handleTurn move = do
                    p@(P cl pt) <- getPiece (src move)
                    game <- S.get
                    if (current game) == cl
                        then if (validMove p (src move) (dest move))
                            then movePiece move
                            else throwError $ "Invalid move for " ++ (show pt)
                        else throwError $ "It is " ++ (show $ current game) ++ "'s turn"

-------------------------------------------------------------------------

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


-------------------------------------------------------------------------

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

-------------------------------------------------------------------------

updateBoard :: Piece -> Move -> ChessBoard ()
updateBoard p move = do
                        game <- S.get
                        S.put $ Game (Map.delete (src move) (board game)) (current game)
                        game <- S.get
                        S.put $ Game (Map.insert (dest move) p (board game)) (current game)

-------------------------------------------------------------------------

movePiece :: Move -> ChessBoard ()
movePiece move = do
                    if (src move) == (dest move)
                        then do
                            game <- S.get
                            S.put $ Game (board game) (otherPlayer (current game))
                        else do
                            specialMove <- handleSpecialCases move
                            if specialMove then return () else do
                            nextImmLoc <- getNextImmLocation move
                            movePieceOnce $ Move (src move) nextImmLoc
                            movePiece $ Move nextImmLoc (dest move)

-------------------------------------------------------------------------

movePieceOnce :: Move -> ChessBoard ()
movePieceOnce move = do
                        pSrc <- getPiece (src move)
                        game <- S.get
                        case (Map.lookup (dest move) (board game)) of
                            Nothing -> case pSrc of
                                           (P _ Pawn) -> case (src move, dest move) of
                                                        (Loc x1 y1,Loc x2 y2) -> if not (x1 == x2)
                                                                                   then throwError $ "Invalid move"
                                                                                   else updateBoard pSrc move
                                           (P _ _)    -> updateBoard pSrc move
                            (Just (P clDest _)) -> if clDest == (current game)
                                                    then throwError $ "Invalid move"
                                                    else case pSrc of
                                                           (P _ Pawn) -> case (src move, dest move) of
                                                                        (Loc x1 y1,Loc x2 y2) -> if (x1 == x2)
                                                                                                   then throwError $ "Invalid move"
                                                                                                   else updateBoard pSrc move
                                                           (P _ _)    -> updateBoard pSrc move

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

handleSpecialCases :: Move -> ChessBoard Bool
handleSpecialCases move = do
                            p <- getPiece (src move)
                            game <- S.get
                            case (p, (src move, dest move)) of 
                                (P cl King, (s, d)) -> if not $ isCastling s d
                                                        then if (isSafeToMoveForKing game d)
                                                            then do
                                                                    movePieceOnce move
                                                                    return True
                                                            else throwError $ "Not safe for king!"
                                                        else doCastlingIfAllowed s d
                                -- undefined - handle other special cases
                                otherwise -> return False

                            where
                            -- does king move two steps? - allow this in validMove as well (undefined)
                            isCastling :: Location -> Location -> Bool
                            isCastling (Loc x1 y1) (Loc x2 y2) = (y1==y2) && (abs (x1-x2)==2)

                            doCastlingIfAllowed :: Location -> Location -> ChessBoard Bool
                            doCastlingIfAllowed s d = undefined

-------------------------------------------------------------------------

-- look for checkmate cases / tie cases

-- assuming that the king will never be removed - a checkmate case
-- will be reported as win and the game should end there itself.
-- so before making a move, check whether its a checkmate

-- it's my turn ....
-- have i been checked?
-- have i lost the game?
-- is there no safe chance left, but my current position?

checkGameStatus :: Game -> GameStatus
checkGameStatus game = if (isCheck game)
                        then if (isSafeMoveAvailableForKing game) || (canSomePieceDefendKing game)
                            then Checked
                            else gameLost (current game)
                        else if (isSafeMoveAvailableForKing game) || (somePieceCanMove game)
                            then Playing
                            else Tie

isCheck :: Game -> Bool
isCheck game = any tryAttack oppPieces
            where 

            tryAttack :: Location -> Bool
            tryAttack loc = case (runStateT (handleTurn (Move loc (kingLocation game))) game) of
                            Left _ -> False
                            otherwise -> True

            oppPieces :: [Location]
            oppPieces = filter isOppColor (Map.keys (board game))

            isOppColor :: Location -> Bool
            isOppColor loc = case (Map.lookup loc (board game)) of
                                (Just (P (col) _)) | (col==(current game)) -> False
                                otherwise -> True

isSafeMoveAvailableForKing :: Game -> Bool
isSafeMoveAvailableForKing game = any (isSafeToMoveForKing game) (kingAvailableMoves game)

-- can the king move safely to the given location
isSafeToMoveForKing :: Game -> Location -> Bool
isSafeToMoveForKing game loc = not $ isCheck $ Game moveKing (current game)
                where
                moveKing :: Board
                moveKing = Map.insert loc (P (current game) King) (Map.delete (kingLocation game) (board game))

-- the set of possible moves a king can take
kingAvailableMoves :: Game -> [Location]
kingAvailableMoves game = foldr (appendIfValidInGameContext (kingLocation game)) [] possibleSteps
                where
                    possibleSteps :: [(Int, Int)]
                    possibleSteps = [(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0)]

                    appendIfValidInGameContext :: Location -> (Int, Int) -> [Location] -> [Location]
                    appendIfValidInGameContext (Loc x y) (a, b) lst = if ((x+a)>0 && (x+a)<9) &&
                                                                         ((y+b)>0 && (y+b)<9) &&
                                                                         sameColorPieceNotPresent (Loc (x+a) (y+b))
                                                                      then (Loc (x+a) (y+b)) : lst
                                                                      else lst
                    sameColorPieceNotPresent :: Location -> Bool
                    sameColorPieceNotPresent loc = case (Map.lookup loc (board game)) of
                                                    (Just (P cl _)) | (cl == (current game)) -> False
                                                    otherwise -> True                                                                      

kingLocation :: Game -> Location
kingLocation game = Map.foldrWithKey (kingFunc) (Loc 1 1) (board game)
                where
                    kingFunc loc (P pl King) _ | pl == (current game) = loc
                    kingFunc _ _ prev = prev

somePieceCanMove :: Game -> Bool
somePieceCanMove game = any (tryMove game) sameColorLocations
                where
                tryMove :: Game -> Location -> Bool
                tryMove game loc = any (canMove game loc) (nextMoveSet game loc)

                canMove :: Game -> Location -> Location -> Bool
                canMove game loc loc' = case (runStateT (handleTurn (Move loc loc')) game) of
                                    Left _ -> False
                                    otherwise -> True

                nextMoveSet :: Game -> Location -> [Location]
                nextMoveSet game loc = filter (\(Loc x y) -> x>0 && x<9 && y<9 && y>0)
                                        $ possibleNextMoves game loc

                possibleNextMoves :: Game -> Location -> [Location]
                possibleNextMoves game loc = case ((Map.lookup loc (board game)), loc) of
                                    (Nothing, _) -> []
                                    (Just (P _ King), Loc x y) -> [] -- already taken care of
                                    (Just (P _ Bishop), Loc x y) -> [Loc (x+1) (y+1), Loc (x-1) (y+1),
                                                                     Loc (x+1) (y-1), Loc (x-1) (y-1)]                                
                                    (Just (P _ Rook), Loc x y) ->   [Loc (x) (y+1), Loc (x-1) (y),
                                                                     Loc (x+1) (y), Loc (x) (y-1)]
                                    (Just (P _ Queen), Loc x y) ->   [Loc (x) (y+1), Loc (x-1) (y),
                                                                     Loc (x+1) (y), Loc (x) (y-1),
                                                                     Loc (x+1) (y+1), Loc (x-1) (y+1),
                                                                     Loc (x+1) (y-1), Loc (x-1) (y-1)]
                                    (Just (P _ Knight), Loc x y) -> [Loc (x-2) (y+1), Loc (x-1) (y+2),
                                                                     Loc (x+1) (y+2), Loc (x+2) (y+1),
                                                                     Loc (x-2) (y-1), Loc (x-1) (y-2),
                                                                     Loc (x+1) (y-2), Loc (x+2) (y-1)]
                                    (Just (P White Pawn), Loc x y) -> [Loc (x-1) (y+1), Loc (x) (y+1),
                                                                        Loc (x+1) (y+1)]
                                    (Just (P Black Pawn), Loc x y) -> [Loc (x-1) (y-1), Loc (x) (y-1),
                                                                        Loc (x+1) (y-1)]                                                                   

                sameColorLocations :: [Location]
                sameColorLocations = filter isSameColor (Map.keys (board game))

                isSameColor :: Location -> Bool
                isSameColor loc = case (Map.lookup loc (board game)) of
                        (Just (P (col) _)) | (col==(current game)) -> True
                        otherwise -> False

canSomePieceDefendKing :: Game -> Bool
canSomePieceDefendKing game = undefined

gameLost :: Player -> GameStatus
gameLost White = BlackWins
gameLost Black = WhiteWins

-------------------------------------------------------------------------


-- handle special cases like castling, pawn conversion, en-passant etc
-- also handle scenario when king moves itself to a position where it can
-- be directly killed (use isSafeToMoveForKing)

--current bugs: pawns can capture non-diagonally