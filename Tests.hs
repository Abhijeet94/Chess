{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module Tests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>), choose)
  
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
instance Arbitrary Location where
  arbitrary = liftM2 Loc (choose (1,8)) (choose (1,8))
  shrink (Loc x y)       = []
-- [QUESTION] Is QuickCheck even worth it in this case because all the cases are incredibly complex

