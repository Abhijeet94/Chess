{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module Tests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..),Gen(..),Property(..),OrderedList(..),
                        forAll,frequency,elements,sized,oneof,(==>),collect,
                        quickCheck,sample,choose,quickCheckWith,
                        classify,stdArgs,maxSuccess)
  
import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import Control.Applicative (Alternative(..),liftA3)
import Control.Monad (liftM, liftM2)

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import qualified Control.Monad.State as S

import GameLogic
import PlayChess
import TransC

-------------------------------------------------------------------------
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
                        (Loc 5 2, P White King),  
                        (Loc 5 7, P Black King), 
                        (Loc 8 2, P Black Pawn)
                        ]

bishopGame :: Game
bishopGame = Game (Map.fromList pos) White []
                where
                pos :: [(Location, Piece)]
                pos =  [(Loc 3 3, P White Bishop),
                        (Loc 5 1, P White King),  
                        (Loc 5 8, P Black King), 
                        (Loc 3 6, P Black Bishop)
                        ]
                      
rookGame :: Game
rookGame = Game (Map.fromList pos) White []
                where
                pos :: [(Location, Piece)]
                pos =  [(Loc 3 3, P White Rook),
                        (Loc 5 1, P White King),  
                        (Loc 5 8, P Black King), 
                        (Loc 3 6, P Black Rook)
                        ]

queenGame :: Game
queenGame = Game (Map.fromList pos) White []
                where
                pos :: [(Location, Piece)]
                pos =  [(Loc 3 3, P White Queen),
                        (Loc 5 1, P White King),  
                        (Loc 5 8, P Black King), 
                        (Loc 3 6, P Black Queen)
                        ]
                        
-- this basically simulates playing a game except instead of printing
-- it returns a Game so we can test the implementation rather than 
-- the interface
playGameTest :: Game -> [String] -> Game
playGameTest game [] = game
playGameTest game (input:xs) = do
                                case (getNextMove input) of
                                  Nothing -> do
                                              playGameTest game (input:xs)
                                  (Just move) -> case (runStateT (handleTurn move) game) of
                                                  Left s -> game
                                                  Right (_, game') -> playGameTest game' xs

-- ALL TESTS
allTests :: Test
allTests = TestList [vMoveTests,pawnTests,
                     knightTests,bishopTests,
                     rookTests,queenTests,
                     kingTests,gameStatusTests,
                     gameTests]

                                                   
-- validMove/invalidMove tests except pawn
vMoveTests :: Test
vMoveTests = TestList [tValidMove,tInvalidMove]

tValidMove :: Test
tValidMove = "validMove tests" ~: TestList [
      validMove (P White King) (Loc 4 4) (Loc 5 5) ~?= True,
      validMove (P White King) (Loc 4 4) (Loc 4 5) ~?= True,
      validMove (P White King) (Loc 4 4) (Loc 3 5) ~?= True,
      validMove (P White King) (Loc 4 4) (Loc 3 4) ~?= True,
      validMove (P White Queen) (Loc 4 4) (Loc 1 7) ~?= True,
      validMove (P White Queen) (Loc 4 4) (Loc 8 8) ~?= True,
      validMove (P White Queen) (Loc 4 4) (Loc 4 1) ~?= True,
      validMove (P White Queen) (Loc 4 4) (Loc 1 4) ~?= True,
      validMove (P White Bishop) (Loc 4 4) (Loc 1 7) ~?= True,
      validMove (P White Bishop) (Loc 4 4) (Loc 8 8) ~?= True,
      validMove (P White Rook) (Loc 4 4) (Loc 4 8) ~?= True,
      validMove (P White Rook) (Loc 4 4) (Loc 8 4) ~?= True,
      validMove (P White Knight) (Loc 4 4) (Loc 3 6) ~?= True,
      validMove (P White Knight) (Loc 4 4) (Loc 6 3) ~?= True
      ]

tInvalidMove :: Test
tInvalidMove = "invalidMove tests" ~: TestList [
      validMove (P White King) (Loc 4 4) (Loc 5 6) ~?= False,
      validMove (P White King) (Loc 4 4) (Loc 2 5) ~?= False,
      validMove (P White King) (Loc 4 4) (Loc 7 5) ~?= False,
      validMove (P White King) (Loc 4 4) (Loc 1 4) ~?= False,
      validMove (P White Queen) (Loc 4 4) (Loc 3 7) ~?= False,
      validMove (P White Queen) (Loc 4 4) (Loc 5 8) ~?= False,
      validMove (P White Queen) (Loc 4 4) (Loc 2 3) ~?= False,
      validMove (P White Queen) (Loc 4 4) (Loc 1 3) ~?= False,
      validMove (P White Bishop) (Loc 4 4) (Loc 4 7) ~?= False,
      validMove (P White Bishop) (Loc 4 4) (Loc 8 4) ~?= False,
      validMove (P White Rook) (Loc 4 4) (Loc 8 8) ~?= False,
      validMove (P White Rook) (Loc 4 4) (Loc 1 1) ~?= False,
      validMove (P White Knight) (Loc 4 4) (Loc 4 6) ~?= False,
      validMove (P White Knight) (Loc 4 4) (Loc 5 3) ~?= False
      ]
                    
-- pawn tests
pawnTests :: Test
pawnTests = TestList [tPawn1Step,tPawn2Step,
                      tPawnNo2StepAfterFirstMove,tPawnEnPassant,
                      tPawnEnPassantFailure,tPawnNoBackwardsOrSideways,
                      tPawnNoSideways,tPawnXDiag,
                      tPawnNoDiagIfNoX,tPawnPromotion]
                      
tPawn1Step :: Test
tPawn1Step = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 3) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 6) b
             ] where b = board (playGameTest initialGame ["E2 E3","D7 D6"])
                    

tPawn2Step :: Test
tPawn2Step = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 4) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest initialGame ["E2 E4","D7 D5"])

tPawnNo2StepAfterFirstMove :: Test
tPawnNo2StepAfterFirstMove = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 4) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest initialGame ["E2 E4", "D7 D5",
                                                          "E4 E6","D5 D3"])

tPawnEnPassant :: Test
tPawnEnPassant = TestList [
                Nothing ~?= Map.lookup (Loc 4 5) b,
                Just (P White Pawn) ~?= Map.lookup (Loc 4 6) b
             ] where b = board (playGameTest initialGame ["E2 E4","F7 F5",
                                                          "E4 E5","D7 D5",
                                                          "E5 D6"])

-- can't en passant if it's not the immediate turn
tPawnEnPassantFailure :: Test
tPawnEnPassantFailure = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 5) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 5) b,
                Nothing ~?= Map.lookup (Loc 4 6) b
             ] where b = board (playGameTest initialGame ["E2 E4","D7 D5",
                                                          "E4 E5","F7 F5",
                                                          "E5 D6"])
tPawnNoBackwardsOrSideways :: Test
tPawnNoBackwardsOrSideways = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 4) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest initialGame ["E2 E4","D7 D5",
                                                          "E4 E3","D5 D6"])

tPawnNoSideways :: Test
tPawnNoSideways = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 4) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest initialGame ["E2 E4","D7 D5",
                                                          "E4 D4","D5 E5"])
                        
tPawnXDiag :: Test
tPawnXDiag = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest initialGame ["E2 E4","D7 D5",
                                                          "E4 D5"])
                                                          
tPawnNoDiagIfNoX :: Test
tPawnNoDiagIfNoX = TestList [
                Just (P White Pawn) ~?= Map.lookup (Loc 5 2) b,
                Just (P Black Pawn) ~?= Map.lookup (Loc 4 7) b
             ] where b = board (playGameTest initialGame ["E2 D3","D7 E6"])
                                                         
tPawnPromotion :: Test
tPawnPromotion = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 1 8) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 8 1) b
             ] where b = board (playGameTest promoteGame ["A7 A8","H2 H1"])
        
-- knight tests        
knightTests :: Test
knightTests = TestList [tKnightMove1,tKnightMove2,
                        tKnightMove3,tKnightMove4]
                        
tKnightMove1 :: Test
tKnightMove1 = TestList [
                Just (P White Knight) ~?= Map.lookup (Loc 3 3) b,
                Just (P Black Knight) ~?= Map.lookup (Loc 6 6) b
             ] where b = board (playGameTest initialGame ["B1 C3","G8 F6"])

tKnightMove2 :: Test
tKnightMove2 = TestList [
                Just (P White Knight) ~?= Map.lookup (Loc 2 5) b,
                Just (P Black Knight) ~?= Map.lookup (Loc 7 4) b
             ] where b = board (playGameTest initialGame ["B1 C3","G8 F6",
                                                          "C3 B5","F6 G4"]) 
tKnightMove3 :: Test
tKnightMove3 = TestList [
                Just (P White Knight) ~?= Map.lookup (Loc 4 4) b,
                Just (P Black Knight) ~?= Map.lookup (Loc 5 5) b
             ] where b = board (playGameTest initialGame ["B1 C3","G8 F6",
                                                          "C3 B5","F6 G4",
                                                          "B5 D4","G4 E5"]) 
                                                          
tKnightMove4 :: Test
tKnightMove4 = TestList [
                Just (P White Knight) ~?= Map.lookup (Loc 5 6) b,
                Just (P Black Knight) ~?= Map.lookup (Loc 4 3) b
             ] where b = board (playGameTest initialGame ["B1 C3","G8 F6",
                                                          "C3 B5","F6 G4",
                                                          "B5 D4","G4 E5",
                                                          "D4 E6","E5 D3"]) 
                                                          
-- bishop tests  
bishopTests :: Test
bishopTests = TestList [tBishopMoveNW,tBishopMoveNE,
                        tBishopMoveSW,tBishopMoveSE,
                        tBishopCantGoThroughSameColor]                                                        
tBishopMoveNW :: Test
tBishopMoveNW = TestList [
                Just (P White Bishop) ~?= Map.lookup (Loc 1 5) b,
                Just (P Black Bishop) ~?= Map.lookup (Loc 1 8) b
             ] where b = board (playGameTest bishopGame ["C3 A5","C6 A8"]) 

tBishopMoveNE :: Test
tBishopMoveNE = TestList [
                Just (P White Bishop) ~?= Map.lookup (Loc 4 4) b,
                Just (P Black Bishop) ~?= Map.lookup (Loc 4 7) b
             ] where b = board (playGameTest bishopGame ["C3 D4","C6 D7"]) 

tBishopMoveSW :: Test
tBishopMoveSW = TestList [
                Just (P White Bishop) ~?= Map.lookup (Loc 1 1) b,
                Just (P Black Bishop) ~?= Map.lookup (Loc 1 4) b
             ] where b = board (playGameTest bishopGame ["C3 A1","C6 A4"]) 

tBishopMoveSE :: Test
tBishopMoveSE = TestList [
                Just (P White Bishop) ~?= Map.lookup (Loc 4 2) b,
                Just (P Black Bishop) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest bishopGame ["C3 D2","C6 D5"]) 

tBishopCantGoThroughSameColor :: Test
tBishopCantGoThroughSameColor = TestList [
                Just (P White Bishop) ~?= Map.lookup (Loc 3 3) b,
                Just (P Black Bishop) ~?= Map.lookup (Loc 3 6) b
             ] where b = board (playGameTest bishopGame ["E1 D2","E8 D7",
                                                         "C3 E1","C6 E8"]) 

-- rook tests
rookTests :: Test
rookTests = TestList [tRookMoveN,tRookMoveS,
                      tRookMoveW,tRookMoveE,
                      tRookCantGoThroughSameColor]    
tRookMoveN :: Test
tRookMoveN = TestList [
                Just (P White Rook) ~?= Map.lookup (Loc 3 5) b,
                Just (P Black Rook) ~?= Map.lookup (Loc 3 8) b
             ] where b = board (playGameTest rookGame ["C3 C5","C6 C8"]) 

tRookMoveS :: Test
tRookMoveS = TestList [
                Just (P White Rook) ~?= Map.lookup (Loc 3 1) b,
                Just (P Black Rook) ~?= Map.lookup (Loc 3 3) b
             ] where b = board (playGameTest rookGame ["C3 C1","C6 C3"]) 

tRookMoveW :: Test
tRookMoveW = TestList [
                Just (P White Rook) ~?= Map.lookup (Loc 1 3) b,
                Just (P Black Rook) ~?= Map.lookup (Loc 1 6) b
             ] where b = board (playGameTest rookGame ["C3 A3","C6 A6"]) 

tRookMoveE :: Test
tRookMoveE = TestList [
                Just (P White Rook) ~?= Map.lookup (Loc 6 3) b,
                Just (P Black Rook) ~?= Map.lookup (Loc 6 6) b
             ] where b = board (playGameTest rookGame ["C3 F3","C6 F6"]) 

tRookCantGoThroughSameColor :: Test
tRookCantGoThroughSameColor = TestList [
                Just (P White Rook) ~?= Map.lookup (Loc 3 1) b,
                Just (P Black Rook) ~?= Map.lookup (Loc 3 8) b
             ] where b = board (playGameTest rookGame ["C3 C1","C6 C8",
                                                       "C1 F1","C8 F8"]) 


-- queen tests
queenTests :: Test
queenTests = TestList [tQueenMoveN,tQueenMoveS,
                      tQueenMoveW,tQueenMoveE,
                      tQueenMoveNW,tQueenMoveNE,
                      tQueenMoveSW,tQueenMoveSE,
                      tQueenCantGoThroughSameColor] 
 
tQueenMoveN :: Test
tQueenMoveN = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 3 5) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 3 8) b
             ] where b = board (playGameTest queenGame ["C3 C5","C6 C8"]) 

tQueenMoveS :: Test
tQueenMoveS = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 3 1) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 3 3) b
             ] where b = board (playGameTest queenGame ["C3 C1","C6 C3"]) 

tQueenMoveW :: Test
tQueenMoveW = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 1 3) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 1 6) b
             ] where b = board (playGameTest queenGame ["C3 A3","C6 A6"]) 

tQueenMoveE :: Test
tQueenMoveE = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 6 3) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 6 6) b
             ] where b = board (playGameTest queenGame ["C3 F3","C6 F6"]) 
             
tQueenMoveNW :: Test
tQueenMoveNW = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 1 5) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 1 8) b
             ] where b = board (playGameTest queenGame ["C3 A5","C6 A8"]) 

tQueenMoveNE :: Test
tQueenMoveNE = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 4 4) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 4 7) b
             ] where b = board (playGameTest queenGame ["C3 D4","C6 D7"]) 

tQueenMoveSW :: Test
tQueenMoveSW = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 1 1) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 1 4) b
             ] where b = board (playGameTest queenGame ["C3 A1","C6 A4"]) 

tQueenMoveSE :: Test
tQueenMoveSE = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 4 2) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 4 5) b
             ] where b = board (playGameTest queenGame ["C3 D2","C6 D5"]) 

tQueenCantGoThroughSameColor :: Test
tQueenCantGoThroughSameColor = TestList [
                Just (P White Queen) ~?= Map.lookup (Loc 3 1) b,
                Just (P Black Queen) ~?= Map.lookup (Loc 3 8) b
             ] where b = board (playGameTest queenGame ["C3 C1","C6 C8",
                                                       "C1 F1","C8 F8"]) 
                                                       
-- king tests
kingTests :: Test
kingTests = TestList [tKingMoveAllDirs,tKingCantMoveWhenBlocked,
                      tKingCantMoveIntoCheck,tKingCheckBishop,
                      tKingCheckBishop,tKingCheckQueen,
                      tKingCheckKnight,tKingCheckRook,
                      tKingCheckPawn,tKingCastleL,
                      tKingCastleR,tKingNoCastleIfMoved,
                      tKingNoCastleIfRookMoved] 
                      
tKingMoveAllDirs :: Test
tKingMoveAllDirs = TestList [
                    Just (P White King) ~?= Map.lookup (Loc 4 1) b,
                    Just (P Black King) ~?= Map.lookup (Loc 4 8) b
                 ] where b = board (playGameTest castleGame ["E1 E2","E8 E7",
                                                             "E2 D3","E7 D6",
                                                             "D3 C2","D6 C7",
                                                             "C2 C1","C7 C8",
                                                             "C1 D1","C8 D8"]) 

tKingCantMoveWhenBlocked :: Test
tKingCantMoveWhenBlocked = TestList [
                    Just (P White King) ~?= Map.lookup (Loc 4 2) b,
                    Just (P Black King) ~?= Map.lookup (Loc 4 7) b
                 ] where b = board (playGameTest bishopGame ["E1 D2","E8 D7",
                                                             "D2 C3","D7 C6"]) 

tKingCantMoveIntoCheck :: Test
tKingCantMoveIntoCheck = TestList [
                    Just (P White King) ~?= Map.lookup (Loc 5 2) b
                 ] where b = board (playGameTest bishopGame ["E1 E2","E8 D7",
                                                             "E2 F3"]) 

tKingCheckBishop :: Test
tKingCheckBishop = TestList [
                    checkGameStatus g ~?= Checked
                 ] where g = playGameTest bishopGame ["E1 E2","C6 F3"]

tKingCheckQueen :: Test
tKingCheckQueen = TestList [
                    checkGameStatus g ~?= Checked
                 ] where g = playGameTest queenGame ["E1 E2","C6 F3"]

tKingCheckKnight :: Test
tKingCheckKnight = undefined

tKingCheckRook :: Test
tKingCheckRook = undefined

tKingCheckPawn :: Test
tKingCheckPawn = undefined
 
tKingCastleL :: Test 
tKingCastleL = TestList [
                    Just (P White King) ~?= Map.lookup (Loc 3 1) b,
                    Just (P White Rook) ~?= Map.lookup (Loc 4 1) b,
                    Just (P Black King) ~?= Map.lookup (Loc 3 8) b,
                    Just (P Black Rook) ~?= Map.lookup (Loc 4 8) b
                 ] where b = board (playGameTest castleGame ["E1 C1","E8 C8"]) 

tKingCastleR :: Test
tKingCastleR = TestList [
                    Just (P White King) ~?= Map.lookup (Loc 7 1) b,
                    Just (P White Rook) ~?= Map.lookup (Loc 6 1) b,
                    Just (P Black King) ~?= Map.lookup (Loc 7 8) b,
                    Just (P Black Rook) ~?= Map.lookup (Loc 6 8) b
                 ] where b = board (playGameTest castleGame ["E1 G1","E8 G8"]) 


tKingNoCastleIfMoved :: Test
tKingNoCastleIfMoved = TestList [
                    Just (P White King) ~?= Map.lookup (Loc 3 1) b,
                    Just (P White Rook) ~?= Map.lookup (Loc 1 1) b,
                    Just (P Black King) ~?= Map.lookup (Loc 3 8) b,
                    Just (P Black Rook) ~?= Map.lookup (Loc 1 8) b
                 ] where b = board (playGameTest castleGame ["E1 C1","E8 C8"]) 

tKingNoCastleIfRookMoved :: Test
tKingNoCastleIfRookMoved = undefined

tCantMoveOtherPiecesIfChecked :: Test
tCantMoveOtherPiecesIfChecked = undefined

-- tests to check for endgame scenarios 
gameStatusTests :: Test
gameStatusTests = TestList [tCheckPlaying,tCheckMate,tCheckTie]

-- check for still playing
tCheckPlaying :: Test
tCheckPlaying = checkGameStatus g ~?= Playing 
                    where g = playGameTest initialGame gameSimple

-- check for win
tCheckMate :: Test
tCheckMate = checkGameStatus g ~?= WhiteWins 
                    where g = playGameTest initialGame gameMate
                    
-- check for tie
tCheckTie :: Test
tCheckTie = checkGameStatus g ~?= Tie 
                    where g = playGameTest initialGame gameTie 


-- Complex game tests with FakeIO
gameTests :: Test
gameTests = TestList [tInitialBoard,tGameSimple,tGameMate,tGameTie]

-- Simple game from the Wikipedia page on chess
gameSimple :: [String]
gameSimple = ["E2 E4", "E7 E5",
             "G1 F3", "B8 C6",
             "F1 B5", "A7 A6"]
             
-- Game ending in checkmate (White) from
-- https://en.wikibooks.org/wiki/Chess/Sample_chess_game
gameMate :: [String]
gameMate = ["E2 E4", "E7 E5",
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

-- Game ending in stalemate from https://www.youtube.com/watch?v=abB2_Em3Ixo
gameTie :: [String]
gameTie =   ["E2 E3", "A7 A5",
             "D1 H5", "A8 A6",
             "H5 A5", "H7 H5",
             "A5 C7", "A6 H6",
             "H2 H4", "F7 F6",
             "C7 D7", "E8 F7",
             "D7 B7", "D8 D3",
             "B7 B8", "D3 H7",
             "B8 C8", "F7 G6",
             "C8 E6"]             

-- This is for regression testing with printing
tInitialBoard :: Test
tInitialBoard = runFakeIO (printBoard (board initialGame)) [] ~?= ["8 ","BR "," ","BN "," ","BB "," ","BQ "," ","BK "," ","BB "," ","BN "," ","BR ","\n","7 ","BP "," ","BP "," ","BP "," ","BP "," ","BP "," ","BP "," ","BP "," ","BP ","\n","6 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x ","\n","5 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x ","\n","4 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x ","\n","3 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x ","\n","2 ","WP "," ","WP "," ","WP "," ","WP "," ","WP "," ","WP "," ","WP "," ","WP ","\n","1 ","WR "," ","WN "," ","WB "," ","WQ "," ","WK "," ","WB "," ","WN "," ","WR ","\n","   A   B   C   D   E   F   G   H \n"]

tGameSimple :: Test
tGameSimple = runFakeIO (printBoard (board (playGameTest initialGame gameSimple))) [] ~?= ["8 ","BR "," "," x "," ","BB "," ","BQ "," ","BK "," ","BB "," ","BN "," ","BR ","\n","7 "," x "," ","BP "," ","BP "," ","BP "," "," x "," ","BP "," ","BP "," ","BP ","\n","6 ","BP "," "," x "," ","BN "," "," x "," "," x "," "," x "," "," x "," "," x ","\n","5 "," x "," ","WB "," "," x "," "," x "," ","BP "," "," x "," "," x "," "," x ","\n","4 "," x "," "," x "," "," x "," "," x "," ","WP "," "," x "," "," x "," "," x ","\n","3 "," x "," "," x "," "," x "," "," x "," "," x "," ","WN "," "," x "," "," x ","\n","2 ","WP "," ","WP "," ","WP "," ","WP "," "," x "," ","WP "," ","WP "," ","WP ","\n","1 ","WR "," ","WN "," ","WB "," ","WQ "," ","WK "," "," x "," "," x "," ","WR ","\n","   A   B   C   D   E   F   G   H \n"]

tGameMate :: Test
tGameMate = runFakeIO (printBoard (board (playGameTest initialGame gameMate))) [] ~?= ["8 ","BR "," ","BN "," "," x "," "," x "," "," x "," ","BB "," ","BN "," ","BR ","\n","7 ","BP "," ","BB "," ","BP "," "," x "," "," x "," ","WQ "," "," x "," "," x ","\n","6 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," ","BK ","\n","5 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," ","BQ "," ","WR ","\n","4 "," x "," "," x "," "," x "," ","WP "," ","WP "," "," x "," "," x "," "," x ","\n","3 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x ","\n","2 ","WP "," ","WP "," ","WP "," "," x "," "," x "," ","WP "," ","WP "," "," x ","\n","1 ","WR "," ","WN "," ","WB "," "," x "," ","WK "," "," x "," "," x "," "," x ","\n","   A   B   C   D   E   F   G   H \n"]

tGameTie :: Test
tGameTie = runFakeIO (printBoard (board (playGameTest initialGame gameTie))) [] ~?= ["8 "," x "," "," x "," "," x "," "," x "," "," x "," ","BB "," ","BN "," ","BR ","\n","7 "," x "," "," x "," "," x "," "," x "," ","BP "," "," x "," ","BP "," ","BQ ","\n","6 "," x "," "," x "," "," x "," "," x "," ","WQ "," ","BP "," ","BK "," ","BR ","\n","5 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," ","BP ","\n","4 "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," "," x "," ","WP ","\n","3 "," x "," "," x "," "," x "," "," x "," ","WP "," "," x "," "," x "," "," x ","\n","2 ","WP "," ","WP "," ","WP "," ","WP "," "," x "," ","WP "," ","WP "," "," x ","\n","1 ","WR "," ","WN "," ","WB "," "," x "," ","WK "," ","WB "," ","WN "," ","WR ","\n","   A   B   C   D   E   F   G   H \n"]