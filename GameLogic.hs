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
data Game = Game { board :: Board, current :: Player, moveLog :: [Move], promotedPc :: (Maybe PieceType)}
data GameStatus = BlackWins | WhiteWins | Checked | Tie | Playing deriving (Eq, Show)
type ChessBoard = StateT Game (Either String)

------------------------------------------------------------------------- 

initialGame :: Game
initialGame = Game (Map.fromList pos) White [] Nothing
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
                  Nothing -> throwError ("No piece found at location " ++ 
                                          show loc)
                  (Just x) -> return x

-------------------------------------------------------------------------                  
              
-- uses the movelog to get the number of times a piece has moved      
getMoveCount :: Location -> [Move] -> Int
getMoveCount loc [] = 0
getMoveCount loc (x:xs) = if (dest x == loc) then 1 + getMoveCount (src x) xs
                                else getMoveCount loc xs

-------------------------------------------------------------------------       

-- uses the movelog to check if a piece moved in the last turn
movedLastTurn :: Location -> [Move] -> Bool   
movedLastTurn loc [] = False
movedLastTurn loc (x:xs) = if (dest x == loc) then True else False
        
-------------------------------------------------------------------------

-- runs and handles 
handleTurn :: Move -> ChessBoard ()
handleTurn move = do
                    p@(P cl pt) <- getPiece (src move)
                    game <- S.get
                    if (current game) == cl
                      then if (validMove p (src move) (dest move))
                        then movePiece move
                        else throwError $ "Invalid move for " ++ (show pt)
                      else 
                        throwError ("It is " ++ (show $ current game) ++ 
                                    "'s turn")

-------------------------------------------------------------------------

-- takes in a Piece and a proposed Location and returns 
-- whether or not the Piece can move to that Location 
-- according to the rules for each piece
validMove :: Piece -> Location -> Location -> Bool
validMove (P _ King) (Loc x1 y1) (Loc x2 y2) = 
                                     (dist <= 2 && dist > 0) || 
                                     ((x2 == 3 || x2 == 7) &&
                                      (y2 == 8 || y2 == 1) && 
                                       x1 == 5 && y1 == y2)
                                        where dist = (x1 - x2)^2 + (y1 - y2)^2
validMove (P _ Queen) (Loc x1 y1) (Loc x2 y2) = 
                          validMove (P White Bishop) (Loc x1 y1) (Loc x2 y2) ||
                         validMove (P White Rook) (Loc x1 y1) (Loc x2 y2)
validMove (P _ Bishop) (Loc x1 y1) (Loc x2 y2) = 
                     ((abs (x1 - x2)) == (abs (y1 - y2))) && (abs (x1 - x2)) > 0
validMove (P _ Knight) (Loc x1 y1) (Loc x2 y2) =   
                           (((abs (x1 - x2)) == 2) && ((abs (y1 - y2)) == 1)) ||
                           (((abs (y1 - y2)) == 2) && ((abs (x1 - x2)) == 1))
validMove (P _ Rook) (Loc x1 y1) (Loc x2 y2) = 
                                          ((x1 == x2) && (abs (y1 - y2)) > 0) ||
                                          ((y1 == y2) && (abs (x1 - x2)) > 0)
validMove (P White Pawn) (Loc x1 y1) (Loc x2 y2) = 
                                              ((x1 == x2) && ((y2 - y1 == 1) || 
                                              ((y1 == 2) && (y2 - y1 == 2)))) ||
                                              (((abs (x1 - x2)) == 1) && 
                                              (y2 - y1 == 1))
validMove (P Black Pawn) (Loc x1 y1) (Loc x2 y2) = 
                                             ((x1 == x2) && ((y2 - y1 == -1) || 
                                             ((y1 == 7) && (y2 - y1 == -2)))) ||
                                             (((abs (x1 - x2)) == 1) && 
                                               (y2 - y1 == -1))                                                    


-------------------------------------------------------------------------

-- function to switch the current player
otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

-------------------------------------------------------------------------

-- updates the board once given a piece and a move
updateBoard :: Piece -> Move -> ChessBoard ()
updateBoard p move = do
                        game <- S.get
                        S.put $ game { board = 
                                       (Map.delete (src move) (board game)), 
                                       moveLog = (move : (moveLog game)) }
                        game <- S.get
                        S.put $ game { board = 
                                       (Map.insert (dest move) p (board game))}

-------------------------------------------------------------------------

-- checks if king is in check at end of turn
isKingSafeAfterMove :: ChessBoard ()
isKingSafeAfterMove = do
                        game <- S.get
                        if isCheck game
                        then throwError $ "King is not safe"
                        else return ()

-------------------------------------------------------------------------

-- actually handles moving pieces
movePiece :: Move -> ChessBoard ()
movePiece move = do
                    if (src move) == (dest move)
                      then do
                        isKingSafeAfterMove
                        game <- S.get
                        S.put $ game { current = (otherPlayer (current game))}
                      else do
                        game <- S.get
                        specialMove <- hSpecialCases move
                        if specialMove then 
                           movePiece (Move (dest move) (dest move)) 
                        else do
                        nextImmLoc <- getNextImmLocation move
                        case (Map.lookup nextImmLoc (board game)) of 
                          Nothing -> movePieceOnce $ Move (src move) nextImmLoc
                          (Just (P clDest _)) -> 
                              if (clDest == (current game))
                              then throwError $ "Invalid move"
                              else if (not (nextImmLoc == dest move))
                                   then throwError $ "Invalid move"
                                   else 
                                     movePieceOnce $ Move (src move) nextImmLoc
                        movePiece $ Move nextImmLoc (dest move)

-- 1st helper function for movePiece
movePieceOnce :: Move -> ChessBoard ()
movePieceOnce move = do
                       pSrc <- getPiece (src move)
                       game <- S.get
                       case (Map.lookup (dest move) (board game)) of
                         Nothing -> updateBoard pSrc move
                         (Just (P clDest _)) -> if clDest == (current game) 
                                                then throwError $ "Invalid move"
                                                else updateBoard pSrc move

-- 2nd helper function for movePiece
getNextImmLocation :: Move -> ChessBoard Location
getNextImmLocation move = do 
                            pSrc <- getPiece (src move)
                            return $ getINLoc pSrc (src move) (dest move) 

-- 3rd helper function for movePiece
getINLoc :: Piece -> Location -> Location -> Location
getINLoc (P _ Bishop) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
                                                        where x' = if x2 > x1
                                                                    then x1 + 1
                                                                    else x1 - 1
                                                              y' = if y2 > y1
                                                                    then y1 + 1
                                                                    else y1 - 1

getINLoc (P _ Rook) l1@(Loc x1 y1) l2@(Loc x2 y2) =  Loc x' y'
                                                        where x' = if x2 > x1
                                                                    then x1 + 1
                                                                    else 
                                                                     if x2 == x1
                                                                     then x1
                                                                     else x1 - 1
                                                              y' = if y2 > y1
                                                                    then y1 + 1
                                                                    else 
                                                                     if y2 == y1
                                                                     then y1
                                                                     else y1 - 1                                                                                
getINLoc (P _ King) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
getINLoc (P _ Knight) l1@(Loc x1 y1) l2@(Loc x2 y2) =  l2
getINLoc (P _ Pawn) l1@(Loc x1 y1) l2@(Loc x2 y2) =  if x1 /= x2
                                                     then l2
                                                     else 
                                                       if (abs (y2 - y1)) == 1
                                                       then l2
                                                       else Loc x1 (y1 + (y2-y1))
getINLoc (P _ Queen) l1@(Loc x1 y1) l2@(Loc x2 y2) = Loc x' y'
                                                        where x' = if x2 > x1
                                                                   then x1 + 1
                                                                   else 
                                                                    if x2 == x1
                                                                    then x1
                                                                    else x1 - 1
                                                              y' = if y2 > y1
                                                                    then y1 + 1
                                                                    else 
                                                                     if y2 == y1
                                                                     then y1
                                                                     else y1 - 1                                                                      

-------------------------------------------------------------------------

-- handles all special movement cases such as castling and en passant
-- all pawn movement / promotions / capturing are special cases that 
-- are handled here
hSpecialCases :: Move -> ChessBoard Bool
hSpecialCases move = do
                        p <- getPiece (src move)
                        game <- S.get
                        case (p, (src move, dest move)) of 
                            (P cl King, (s, d)) -> if isCastling s d
                                                   then doCastlingIfAllowed s d
                                                   else return False
                            (P cl Pawn, (s@(Loc x1 y1), d@(Loc x2 y2))) -> 
                                case (Map.lookup (dest move) (board game)) of
                                 Nothing -> 
                                     if not (x1 == x2)
                                     then doEnPassant cl s d
                                     else moveStraight cl s d move     
                                 (Just (P clDest _)) -> 
                                     if clDest == cl
                                     then throwError $ "Invalid move for Pawn"
                                     else 
                                       if (x1 == x2)
                                       then throwError $ "Invalid move for Pawn"
                                       else do 
                                         movePieceOnce move
                                         return True
                            otherwise -> return False

-- determines if king is about to castle based on the move inputted
isCastling :: Location -> Location -> Bool
isCastling (Loc x1 y1) (Loc x2 y2) = (y1==y2) && (abs (x1-x2)==2)

-- handle castling if both the involved rook and king have not moved
doCastlingIfAllowed :: Location -> Location -> ChessBoard Bool
doCastlingIfAllowed (Loc x1 y1) (Loc x2 y2) = 
    do
      game <- S.get
      if (x1 < x2) then -- king-side castling
        case ((Map.lookup (Loc (x1 + 1) y1) (board game)),
              (Map.lookup (Loc (x1 + 2) y1) (board game)),
              (Map.lookup (Loc (x1 + 3) y1) (board game))) of
                (Nothing, Nothing, Just (P _ Rook)) -> 
                    if ((getMoveCount (Loc (x1 + 3) y1) (moveLog game) == 0) && 
                      (getMoveCount (Loc x1 y1) (moveLog game) == 0)) then 
                        do
                            movePieceOnce $ Move (Loc x1 y1) (Loc x2 y2)
                            movePieceOnce $ Move (Loc (x1+3) y1) (Loc (x2-1) y2)
                            return True
                    else throwError $ "Invalid move for King"
                _  -> throwError $ "Can't castle over pieces!"
      else -- queen-side castling
        case ((Map.lookup (Loc (x1 - 1) y1) (board game)),
              (Map.lookup (Loc (x1 - 2) y1) (board game)),
              (Map.lookup (Loc (x1 - 3) y1) (board game)),
              (Map.lookup (Loc (x1 - 4) y1) (board game))) of
                (Nothing, Nothing, Nothing, Just (P _ Rook)) -> 
                    if ((getMoveCount (Loc (x1 - 4) y1) (moveLog game) == 0) && 
                       (getMoveCount (Loc x1 y1) (moveLog game) == 0)) then 
                        do
                            movePieceOnce $ Move (Loc x1 y1) (Loc x2 y2)
                            movePieceOnce $ Move (Loc (x1-4) y1) (Loc (x2+1) y2)
                            return True
                    else throwError $ "Invalid move for King"
                _  -> throwError $ "Can't castle over pieces!"
      
-- handles pawn movement         
moveStraight :: Player -> Location -> Location -> Move -> ChessBoard Bool
moveStraight cl s@(Loc x1 y1) d@(Loc x2 y2) move = do
                                                    movePieceOnce move
                                                    if (y2 == 1) || (y2 == 8) 
                                                        then promotePawn cl s d
                                                        else return True

-- promotes a pawn to another piece specified by user input
promotePawn :: Player -> Location -> Location -> ChessBoard Bool
promotePawn cl (Loc x1 y1) (Loc x2 y2) = do 
  game <- S.get
  case (promotedPc game) of
    Nothing -> throwError $ "getPawnPromotionPiece"
    (Just pc) -> do
        S.put $ game { board = (Map.insert (Loc x2 y2) (P cl pc) (board game)) }
        game <- S.get
        S.put $ game { board = (Map.delete (Loc x1 y1) (board game)) }
        return True

-- handles en passant iff the pawn is in proper position and the enemy pawn
-- has not moved before this turn
doEnPassant :: Player -> Location -> Location -> ChessBoard Bool
doEnPassant cl (Loc x1 y1) (Loc x2 y2) = do
  game <- S.get
  case cl of 
    Black -> 
        if (y2 == 3) then
            case (Map.lookup (Loc x2 y1) (board game)) of
                Just (P White Pawn) -> 
                    if (getMoveCount (Loc x2 y1) (moveLog game) == 1 &&
                       movedLastTurn (Loc x2 y1) (moveLog game) == True) then 
                        do
                            movePieceOnce $ Move (Loc x1 y1) (Loc x2 y1)
                            movePieceOnce $ Move (Loc x2 y1) (Loc x2 y2)
                            return True
                    else throwError $ "Invalid move for Pawn"
                _ -> throwError $ "Invalid move for Pawn"
        else throwError $ "Invalid move for Pawn"
    White -> 
        if (y2 == 6) then
            case (Map.lookup (Loc x2 y1) (board game)) of
                Just (P Black Pawn) -> 
                    if (getMoveCount (Loc x2 y1) (moveLog game) == 1 &&
                        movedLastTurn (Loc x2 y1) (moveLog game) == True) then 
                        do
                            movePieceOnce $ Move (Loc x1 y1) (Loc x2 y1)
                            movePieceOnce $ Move (Loc x2 y1) (Loc x2 y2)
                            return True
                    else throwError $ "Invalid move for Pawn"
                _ -> throwError $ "Invalid move for Pawn"
         else throwError $ "Invalid move for Pawn"

-- set the promoted piece in the game monad
setPromotedPieceInGame :: (Maybe PieceType) -> ChessBoard ()
setPromotedPieceInGame Nothing = do
                            game <- S.get
                            S.put $ game {promotedPc = Nothing}
                            return ()
setPromotedPieceInGame pc = do
                            game <- S.get
                            S.put $ game {promotedPc = pc}
                            return ()

-------------------------------------------------------------------------

-- checks if king is in check, if the game has been lost, if there's a tie
-- if none of these cases are true return Playing, which specifies that
-- the game is ongoing
checkGameStatus :: Game -> GameStatus
checkGameStatus game = if (isCheck game)
                        then if ((kingHasSafeMove game) || 
                                 (canPieceDefendKing game))
                            then Checked
                            else gameLost (current game)
                        else if ((kingHasSafeMove game) || 
                                 (somePieceCanMove game))
                            then Playing
                            else Tie

-------------------------------------------------------------------------
-- checks if the king is in check
  
isCheck :: Game -> Bool
isCheck game = (otherPlayerHasKing game) && any tryAttack oppPieces
  where 
  tryAttack :: Location -> Bool
  tryAttack loc = case (runStateT (handleTurn 
                                  (Move loc (kingLocation game))) 
                                  (cp game)) of
                  Left _ -> False
                  otherwise -> True

  oppPieces :: [Location]
  oppPieces = filter isOppColor (Map.keys (board game))

  isOppColor :: Location -> Bool
  isOppColor loc = case (Map.lookup loc (board game)) of
                      (Just (P (col) _)) | (col==(current game)) -> False
                      otherwise -> True

-- checks if a king has any safe moves 
kingHasSafeMove :: Game -> Bool
kingHasSafeMove game = any (kingCanMoveSafe game) (kingAvailableMoves game)

-- helper function for kingHasSafeMove
kingCanMoveSafe :: Game -> Location -> Bool
kingCanMoveSafe game loc = not $ isCheck $ game { board = moveKing }
  where
  moveKing :: Board
  moveKing = Map.insert loc (P (current game) King) 
                            (Map.delete (kingLocation game) 
                            (board game))

-- the set of all possible moves a king can take
kingAvailableMoves :: Game -> [Location]
kingAvailableMoves game = foldr (appendIfValid (kingLocation game)) [] possibleSteps
  where
  possibleSteps :: [(Int, Int)]
  possibleSteps = [(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0)]

  appendIfValid :: Location -> (Int, Int) -> [Location] -> [Location]
  appendIfValid (Loc x y) (a, b) lst = if ((x+a)>0 && (x+a)<9) &&
                                          ((y+b)>0 && (y+b)<9) &&
                                          noSameColorPiece (Loc (x+a) (y+b))
                                       then (Loc (x+a) (y+b)) : lst
                                       else lst
  noSameColorPiece :: Location -> Bool
  noSameColorPiece loc = case (Map.lookup loc (board game)) of
                          (Just (P cl _)) | (cl == (current game)) -> False
                          otherwise -> True                                                                      

-- gets the location of the current player's king
kingLocation :: Game -> Location
kingLocation game = Map.foldrWithKey (kingFunc) (Loc 1 1) (board game)
  where
  kingFunc loc (P pl King) _ | pl == (current game) = loc
  kingFunc _ _ prev = prev

-- checks if there's an existing move for any piece on the board
-- used to handle stalemate case as no moves remaining = stalemate
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
  possibleNextMoves game loc = 
      case ((Map.lookup loc (board game)), loc) of
          (Nothing, _) -> []
          (Just (P _ King), Loc x y) -> [] -- already handled
          (Just (P _ Bishop), Loc x y) -> [Loc (x+1) (y+1), 
                                           Loc (x-1) (y+1),
                                           Loc (x+1) (y-1), 
                                           Loc (x-1) (y-1)]                                
          (Just (P _ Rook), Loc x y) ->   [Loc (x) (y+1), 
                                           Loc (x-1) (y),
                                           Loc (x+1) (y), 
                                           Loc (x) (y-1)]
          (Just (P _ Queen), Loc x y) ->   [Loc (x) (y+1),  
                                            Loc (x-1) (y),
                                            Loc (x+1) (y), 
                                            Loc (x) (y-1),
                                            Loc (x+1) (y+1), 
                                            Loc (x-1) (y+1),
                                            Loc (x+1) (y-1), 
                                            Loc (x-1) (y-1)]
          (Just (P _ Knight), Loc x y) -> [Loc (x-2) (y+1), 
                                           Loc (x-1) (y+2),
                                           Loc (x+1) (y+2), 
                                           Loc (x+2) (y+1),
                                           Loc (x-2) (y-1), 
                                           Loc (x-1) (y-2),
                                           Loc (x+1) (y-2), 
                                           Loc (x+2) (y-1)]
          (Just (P White Pawn), Loc x y) -> [Loc (x-1) (y+1), 
                                             Loc (x) (y+1),
                                             Loc (x+1) (y+1)]
          (Just (P Black Pawn), Loc x y) -> [Loc (x-1) (y-1), 
                                             Loc (x) (y-1),
                                             Loc (x+1) (y-1)]                                                                   

  sameColorLocations :: [Location]
  sameColorLocations = filter isSameColor (Map.keys (board game))

  isSameColor :: Location -> Bool
  isSameColor loc = case (Map.lookup loc (board game)) of
          (Just (P (col) _)) | (col==(current game)) -> True
          otherwise -> False

-- checks to see if after a move, any piece can block / eliminate a threat 
-- to a checked king by examining all possible moves for all possible pieces
canPieceDefendKing :: Game -> Bool
canPieceDefendKing game = any (tryMove game) sameColorLocations
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

-- change player in game to try configurations for check
cp :: Game -> Game
cp game = game { current = (otherPlayer $ current game) }

-- used before isCheck. One needs to worried about being
-- in check only if the other player has already not lost.
-- this becomes important to check so as to avoid infinite
-- loops when we check for isCheck after every normal move
-- (handleTurn, movePiece etc) as isCheck also uses 
-- handleTurn inside. But this handle turn would have removed
-- the king, because of which the function below will not pass.
otherPlayerHasKing :: Game -> Bool
otherPlayerHasKing game = case foldResult of
                                    (Loc x y) | x == -1 && y == -1 -> False
                                    otherwise -> True
                            where
                                foldResult = (Map.foldrWithKey (kingFunc) 
                                             (Loc (-1) (-1)) (board game))
                                kingFunc loc (P pl King) _ | pl == 
                                             (otherPlayer $ current game) = loc
                                kingFunc _ _ prev = prev

-------------------------------------------------------------------------

-- simply returns a GameStatus corresponding to which player won 
gameLost :: Player -> GameStatus
gameLost White = BlackWins
gameLost Black = WhiteWins

-------------------------------------------------------------------------
