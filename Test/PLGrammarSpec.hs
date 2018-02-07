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
spec = do
  describe "Iso" $ sequence_
    [describe "ParseIso" $ sequence_
      [it "Fails when constructed with const Nothing" pending
      ,it "Succeeds with a constant when constructed with cont (Just constant)" pending
      ,it "Has the same result as the function it was construced with" pending
      ,it "Composing two functions is the same as manually running each" pending
      ,it "(a . b) . c == a . (b . c)" pending
      ]
    ,describe "PrintIso" $ sequence_
      [-- TODO: The same as ParseIso
      ]
    ]
  describe "PLGrammar" $ sequence_
    [
    ]
