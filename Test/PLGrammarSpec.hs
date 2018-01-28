{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
{-|
Module      : PLGrammarSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PLGrammar.
-}
module PLGrammarSpec where

import PLGrammar
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text
import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Coerce

spec :: Spec
spec = describe "PLGrammar" $ sequence_
  [
  ]
