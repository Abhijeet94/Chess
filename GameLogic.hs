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
data Move = Move {src :: Location, dest :: Location} deriving (Eq, Show)
data Game = Game { board :: Board, current :: Player, moveLog :: [Move]}
data GameStatus = BlackWins | WhiteWins | Checked | Tie | Playing
type ChessBoard = StateT Game (Either String)

------------------------------------------------------------------------- 

initialGame :: Game
initialGame = Game (Map.fromList pos) White []
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
                        
castleGame :: Game
castleGame = Game (Map.fromList pos) White []
                where
                pos :: [(Location, Piece)]
                pos =  [(Loc 1 1, P White Rook),
                        (Loc 5 1, P White King),  
                        (Loc 8 1, P White Rook),
                        (Loc 1 8, P Black Rook), 
                        (Loc 5 8, P Black King), 
                        (Loc 8 8, P Black Rook)
                        ]
                        
promoteGame :: Game
promoteGame = Game (Map.fromList pos) White []
                where
                pos :: [(Location, Piece)]
                pos =  [(Loc 1 7, P White Pawn),
                        (Loc 5 1, P White King),  
                        (Loc 5 8, P Black King), 
                        (Loc 8 2, P Black Pawn)
                        ]

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
                    
getMoveCount :: Location -> [Move] -> Int
getMoveCount loc [] = 0
getMoveCount loc (x:xs) = if (dest x == loc) then 1 + getMoveCount (src x) xs
                                else getMoveCount loc xs

-------------------------------------------------------------------------       

movedLastTurn :: Location -> [Move] -> Bool   
movedLastTurn loc [] = False
movedLastTurn loc (x:xs) = if (dest x == loc) then True else False
        
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
validMove (P Black King) (Loc x1 y1) (Loc x2 y2) = (dist <= 2 && dist > 0) || ((x2 == 3 || x2 == 7) && y2 == 8)
                                    where dist = (x1 - x2)^2 + (y1 - y2)^2
validMove (P White King) (Loc x1 y1) (Loc x2 y2) = (dist <= 2 && dist > 0) || ((x2 == 3 || x2 == 7) && y2 == 1)
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
                        S.put $ Game (Map.delete (src move) (board game)) (current game) (move : (moveLog game))
                        game <- S.get
                        S.put $ Game (Map.insert (dest move) p (board game)) (current game) (moveLog game)

-------------------------------------------------------------------------

isKingSafeAfterMove :: ChessBoard ()
isKingSafeAfterMove = do
                        game <- S.get
                        if isCheck game
                        then throwError $ "King is not safe"
                        else return ()

-------------------------------------------------------------------------

movePiece :: Move -> ChessBoard ()
movePiece move = do
                    if (src move) == (dest move)
                        then do
                            isKingSafeAfterMove
                            game <- S.get
                            S.put $ Game (board game) (otherPlayer (current game)) (moveLog game)
                        else do
                            game <- S.get
                            specialMove <- handleSpecialCases move
                            if specialMove then movePiece (Move (dest move) (dest move)) else do
                            nextImmLoc <- getNextImmLocation move
                            case (Map.lookup nextImmLoc (board game)) of 
                                Nothing -> movePieceOnce $ Move (src move) nextImmLoc
                                (Just (P clDest _)) -> if (clDest == (current game))
                                                            then throwError $ "Invalid move"
                                                       else if (not (nextImmLoc == dest move))
                                                                then throwError $ "Invalid move"
                                                            else 
                                                                movePieceOnce $ Move (src move) nextImmLoc
                            movePiece $ Move nextImmLoc (dest move)

-------------------------------------------------------------------------

movePieceOnce :: Move -> ChessBoard ()
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

handleSpecialCases :: Move -> ChessBoard Bool
handleSpecialCases move = do
                            p <- getPiece (src move)
                            game <- S.get
                            case (p, (src move, dest move)) of 
                                (P cl King, (s, d)) -> if isCastling s d
                                                        then doCastlingIfAllowed s d
                                                        else return False
                                (P cl Pawn, ((Loc x1 y1), (Loc x2 y2))) -> 
                                    case (Map.lookup (dest move) (board game)) of
                                        Nothing -> if not (x1 == x2)
                                                       then 
                                                        case cl of --en passant
                                                            Black -> 
                                                                if (y2 == 3) then
                                                                    case (Map.lookup (Loc x2 y1) (board game)) of
                                                                        Just (P White Pawn) -> if (getMoveCount (Loc x2 y1) (moveLog game) == 1 &&
                                                                                                   movedLastTurn (Loc x2 y1) (moveLog game) == True) then 
                                                                                                    do
                                                                                                        movePieceOnce $ Move (Loc x1 y1) (Loc x2 y1)
                                                                                                        movePieceOnce $ Move (Loc x2 y1) (Loc x2 y2)
                                                                                                        return True
                                                                                                    else throwError $ "Invalid move for Pawn"
                                                                        _              -> throwError $ "Invalid move for Pawn"
                                                                else throwError $ "Invalid move for Pawn"
                                                            White -> 
                                                                if (y2 == 6) then
                                                                    case (Map.lookup (Loc x2 y1) (board game)) of
                                                                        Just (P Black Pawn) -> if (getMoveCount (Loc x2 y1) (moveLog game) == 1 &&
                                                                                                   movedLastTurn (Loc x2 y1) (moveLog game) == True) then 
                                                                                                    do
                                                                                                        movePieceOnce $ Move (Loc x1 y1) (Loc x2 y1)
                                                                                                        movePieceOnce $ Move (Loc x2 y1) (Loc x2 y2)
                                                                                                        return True
                                                                                                    else throwError $ "Invalid move for Pawn"
                                                                        _              -> throwError $ "Invalid move for Pawn"
                                                                 else throwError $ "Invalid move for Pawn"
                                                       else 
                                                            do 
                                                                movePieceOnce move
                                                                --REFACTOR THIS 
                                                                if (cl == Black && y2 == 1) then 
                                                                    do 
                                                                        S.put $ Game (Map.insert (Loc x2 y2) (P Black Queen) (board game)) (current game) (moveLog game)
                                                                        game <- S.get
                                                                        S.put $ Game (Map.delete (Loc x1 y1) (board game)) (current game) (moveLog game)
                                                                        return True
                                                                else if (cl == White && y2 == 8) then 
                                                                    do 
                                                                        S.put $ Game (Map.insert (Loc x2 y2) (P White Queen) (board game)) (current game) (moveLog game)
                                                                        game <- S.get
                                                                        S.put $ Game (Map.delete (Loc x1 y1) (board game)) (current game) (moveLog game)
                                                                        return True
                                                                else do 
                                                                        return True
                                                                --END REFACTOR
                                                                --return True
                                        (Just (P clDest _)) -> if clDest == cl
                                                                then throwError $ "Invalid move for Pawn"
                                                                else if (x1 == x2)
                                                                       then throwError $ "Invalid move for Pawn"
                                                                       else do 
                                                                            movePieceOnce move
                                                                            return True

                                -- undefined - handle other special cases
                                otherwise -> return False
                            where
                            -- does king move two steps? - allow this in validMove as well (undefined)
                            isCastling :: Location -> Location -> Bool
                            isCastling (Loc x1 y1) (Loc x2 y2) = (y1==y2) && (abs (x1-x2)==2)

                            doCastlingIfAllowed :: Location -> Location -> ChessBoard Bool
                            doCastlingIfAllowed (Loc x1 y1) (Loc x2 y2) = do
                                                                            game <- S.get
                                                                            if (x1 < x2) then -- king-side castling
                                                                                case ((Map.lookup (Loc (x1 + 1) y1) (board game)),
                                                                                      (Map.lookup (Loc (x1 + 2) y1) (board game)),
                                                                                      (Map.lookup (Loc (x1 + 3) y1) (board game))) of
                                                                                        (Nothing, Nothing, Just (P _ Rook)) -> if ((getMoveCount (Loc (x1 + 3) y1) (moveLog game) == 0) && 
                                                                                                                                  (getMoveCount (Loc x1 y1) (moveLog game) == 0)) then 
                                                                                                                                    do
                                                                                                                                        movePieceOnce $ Move (Loc x1 y1) (Loc x2 y2)
                                                                                                                                        movePieceOnce $ Move (Loc (x1+3) y1) (Loc (x2-1) y2)
                                                                                                                                        return True
                                                                                                                        else throwError $ "Invalid move for King"
                                                                                        _                            -> throwError $ "Can't castle when there are pieces in the way!"
                                                                            else case ((Map.lookup (Loc (x1 - 1) y1) (board game)),
                                                                                      (Map.lookup (Loc (x1 - 2) y1) (board game)),
                                                                                      (Map.lookup (Loc (x1 - 3) y1) (board game)),
                                                                                      (Map.lookup (Loc (x1 - 4) y1) (board game))) of
                                                                                        (Nothing, Nothing, Nothing, Just (P _ Rook)) -> if ((getMoveCount (Loc (x1 - 4) y1) (moveLog game) == 0) && 
                                                                                                                                           (getMoveCount (Loc x1 y1) (moveLog game) == 0)) then 
                                                                                                                                            do
                                                                                                                                                movePieceOnce $ Move (Loc x1 y1) (Loc x2 y2)
                                                                                                                                                movePieceOnce $ Move (Loc (x1-4) y1) (Loc (x2+1) y2)
                                                                                                                                                return True
                                                                                                                                        else throwError $ "Invalid move for King"
                                                                                        _                            -> throwError $ "Can't castle when there are pieces in the way!"
                                                                                  

                        
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
isCheck game = (doesOppPlayerHaveAKingToContinueGame game) && any tryAttack oppPieces
            where 

            tryAttack :: Location -> Bool
            tryAttack loc = case (runStateT (handleTurn (Move loc (kingLocation game))) (cp game)) of
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
isSafeToMoveForKing game loc = not $ isCheck $ Game moveKing (current game) (moveLog game)
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
canSomePieceDefendKing game = any (tryMove game) sameColorLocations
                where
                tryMove :: Game -> Location -> Bool
                tryMove game loc = any (canMove game loc) (nextMoveSet)

                canMove :: Game -> Location -> Location -> Bool
                canMove game loc loc' = case (runStateT (handleTurn (Move loc loc')) game) of
                                    Left _ -> False
                                    Right (_, game') -> True && (not $ isCheck (cp game'))

                nextMoveSet :: [Location]
                nextMoveSet = [Loc x y | x <- [1..8], y <- [1..8]]                                                           

                sameColorLocations :: [Location]
                sameColorLocations = filter isSameColor (Map.keys (board game))

                isSameColor :: Location -> Bool
                isSameColor loc = case (Map.lookup loc (board game)) of
                        (Just (P (col) _)) | (col==(current game)) -> True
                        otherwise -> False

-- Change player in game to try configurations for check etc
cp :: Game -> Game
cp game = Game (board game) (otherPlayer $ current game) (moveLog game)

-- used before isCheck. One needs to worried about being
-- in check only if the other player has already not lost.
-- This becomes important to check so as to avoid infinite
-- loop when we check for isCheck after every normal move
-- (handleTurn, movePiece etc) as isCheck also uses 
-- handleTurn inside. But this handle turn would have removed
-- the king, because of which the function below will not pass.
doesOppPlayerHaveAKingToContinueGame :: Game -> Bool
doesOppPlayerHaveAKingToContinueGame game = case (Map.foldrWithKey (kingFunc) (Loc (-1) (-1)) (board game)) of
                                                    (Loc x y) | x == -1 && y == -1 -> False
                                                    otherwise -> True
                                            where
                                                kingFunc loc (P pl King) _ | pl == (otherPlayer $ current game) = loc
                                                kingFunc _ _ prev = prev

gameLost :: Player -> GameStatus
gameLost White = BlackWins
gameLost Black = WhiteWins

-------------------------------------------------------------------------


-- handle special cases like castling, pawn conversion, en-passant etc
-- also handle scenario when king moves itself to a position where it can
-- be directly killed (use isSafeToMoveForKing)

--current bugs: pawns can capture non-diagonally