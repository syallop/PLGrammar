{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLGrammar.Iso
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

Partial Isomorphisms. Objects which translate back and forth between two types
with the possibility for failure. Round trips should not fail.
The two functions are named parse and print after their most frequent use.
-}
module PLGrammar.Iso
  ( Iso(..)
  , parseIso
  , printIso
  , inverseIso

  , identityI
  , composeIso

  , nilI
  , consI

  , unitI
  , flipI

  , elementIso
  , ignoreIso
  )
  where

import Prelude hiding ((.),id)

import Control.Category
import Control.Monad
import Data.Monoid
import Data.Text hiding (reverse)

-- | An Iso converts both ways between a and b, each with an opportunity for
-- failure.
data Iso a b = Iso
  {_isoLabel :: [Text]
  ,_parseIso :: a -> Maybe b
  ,_printIso :: b -> Maybe a
  }

instance Category Iso where
  g . f = Iso (isoLabel g <> isoLabel f)
              (parseIso f >=> parseIso g)
              (printIso g >=> printIso f)
  id = Iso ["id"] Just Just

isoLabel :: Iso a b -> [Text]
isoLabel (Iso labels _ _) = labels

parseIso :: Iso a b -> a -> Maybe b
parseIso (Iso _ parseI _) = parseI

printIso :: Iso a b -> b -> Maybe a
printIso (Iso _ _ printI) = printI

inverseIso :: Iso a b -> Iso b a
inverseIso (Iso labels parseI printI) = Iso (reverse labels) printI parseI

-- A boring Iso which translates something to itself and back
identityI :: Iso a a
identityI = Iso ["identity"] Just Just

-- Isos can be composed
composeIso :: Iso a b -> Iso b c -> Iso a c
composeIso (Iso labelA parseA printB) (Iso labelB parseB printC)
  = Iso (labelA <> labelB)
        (parseA >=> parseB)
        (printC >=> printB)

nilI :: Iso () [a]
nilI = Iso
  ["nil"]
  (\() -> Just [])
  (\xs -> case xs of
    []     -> Just ()
    (x:xs) -> Nothing
  )

consI :: Iso (a,[a]) [a]
consI = Iso
  ["cons"]
  (\(x,xs) -> Just (x:xs))
  (\xs -> case xs of
    []     -> Nothing
    (x:xs) -> Just (x,xs)
  )

-- Unit element for products
unitI :: Iso a (a,())
unitI = Iso
  ["unit"]
  (\a -> Just (a,()))
  (\(a,()) -> Just a)

-- Products commute
flipI :: Iso (a,b) (b,a)
flipI = Iso
  ["flip"]
  (\(a,b) -> Just (b,a))
  (\(b,a) -> Just (a,b))

elementIso :: Eq a => a -> Iso () a
elementIso a0 = Iso
  ["element","constant-parse","print-equal"]
  (const . Just $ a0)
  (\a1 -> if a0 == a1 then Just () else Nothing)

ignoreIso :: a -> Iso a ()
ignoreIso a = Iso
  ["ignore"]
  (const $ Just ())
  (const $ Just a)

