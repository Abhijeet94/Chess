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

-------------------------------------------------------------------------

-- [QUESTION] How do we test this?
-- [QUESTION] Arbitrary instances for Location, Piece, Board?
-- [QUESTION] Is QuickCheck even worth it in this case because all the cases are incredibly complex
instance Arbitrary Location where
  arbitrary = liftM2 Loc (choose (1,8)) (choose (1,8))
  shrink (Loc x y)       = []

instance Arbitrary Piece where
  arbitrary = liftM2 P (elements [White,Black]) (elements[King,Queen,Bishop,Knight,Rook,Pawn])
  shrink (P player pType)       = []

-- test to check for endgame scenarios 
-- check for player win
-- check for tie
-- check for non-endgame
testCheckEnd :: Test
testCheckEnd = undefined 
    
-- test that moving pieces changes the game state in the way it's supposed to
-- capturing should remove the captured piece from the board
-- castling should be possible
-- important to remember that movePiece is only called when validMove is verified so this literally just
-- moves the piece
testMovePiece :: Test
testMovePiece = undefined

-- This test makes a board, then checks to see if getPiece works by indexing into the Board at a Location 
testGetPiece :: Test
testGetPiece = undefined

-- [QUESTION] How do we test this?  Is this possible to QuickCheck?  Do we even have to test this?
-- make sure the board is printed the way we want it: create a board, input the text that's printed, and check if they match
testPrettyPrint :: Test
testPrettyPrint = undefined


-- inputToLocation tests, make sure correct format is accepted
-- maybe QuickCheck this?
tInputToLocation :: Test
tInputToLocation = "inputToLoc tests" ~: TestList [
      inputToLocation "E4 E5" ~?= Just (Loc 5 4, Loc 5 5),
      inputToLocation "F4 C5" ~?= Just (Loc 6 4, Loc 3 5),
      inputToLocation "A3 B5" ~?= Just (Loc 1 3, Loc 2 5),
      inputToLocation "G4 J5" ~?= Nothing,
      inputToLocation "X4 C5" ~?= Nothing,
      inputToLocation "F9 C5" ~?= Nothing,
      inputToLocation "F0 C5" ~?= Nothing,
      inputToLocation "F1 C9" ~?= Nothing,
      inputToLocation "F1 C0" ~?= Nothing,
      inputToLocation "F1 " ~?= Nothing,
      inputToLocation "x C4" ~?= Nothing,
      inputToLocation "F1 C" ~?= Nothing
      ]

-- validMove/invalidMove tests
-- TODO: expand to have more interesting cases
-- TODO: check pawns
-- [QUESTION] How would we generate interesting cases for this if we were to use QuickCheck?
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




