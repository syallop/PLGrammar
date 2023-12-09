{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE
    GADTs
  , FlexibleContexts
  , KindSignatures
  , OverloadedStrings
  , RankNTypes
  , RankNTypes
  , LambdaCase
  , ScopedTypeVariables
  , TypeOperators
  #-}
{-|
Module      : PLGrammar
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

A description of a grammar. The intent is that it can be used as either a
parser or a printer depending on context. This could prevent round-trip
properties from being accidentally violated for example, by adjusting a parser
but not the printer.

Note: WIP. Some constructors may be missing/ some may be unneccessary and in
general the entire type is likely to change.

Correspondance to invertable-syntax:
  - <$>:   rmap:   \$/
  - <*>:   rap:    \*/
  - <|>:   ralt:   \|/
  - empty: rempty
  - pure:  rpure
  - token: anyChar
-}
module PLGrammar
  ( Grammar ()
  , GrammarInstr (..)

  , charIs
  , textIs

  , anyChar
  , charWhen

  , try
  , label

  , upper
  , lower
  , digit

  , arrow
  , bar
  , star
  , plus
  , comma
  , upArrow
  , lambda
  , langle
  , rangle
  , lparen
  , rparen
  , underscore
  , union
  , question
  , at
  , bigLambda
  , bigAt

  , between
  , betweenParens

  , longestMatching
  , longestMatching1

  , natural

  , alternatives

  , rap
  , (\$/)
  , (\*/)
  , (\|/)
  , (\*)
  , (*/)
  , rmany
  , rmany1

  , spaceAllowed
  , spaceRequired
  , spacePreferred

  , sepBy
  , sepBy1

  , permissive
  , token

  , tokenThenMany1ThenSomething
  )
  where

import Data.Text (Text)
import Data.Char
import Data.Kind
import qualified Data.Text as T

{-import PLGrammar.Iso-}
import PLLabel

import Reversible
import Reversible.Iso
import DSL.Instruction

import Prelude hiding ((.),id)
import Control.Category
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | Instructions which specify core aspects of a Grammar, minus higher level
-- features such as alternatives, mapping, sequencing, etc.
--
-- - 'a' is the result type the Grammar should parse/ print/ act upon.
-- - 'p' is the type of program the Grammar is part of.
data GrammarInstr (p :: Type -> Type) (a :: Type) where
  -- Any character
  GAnyChar
    :: GrammarInstr p Char

  GLabel
    :: Label
    -> p a
    -> GrammarInstr p a

  GTry
    :: p a
    -> GrammarInstr p a

-- | A Grammar is a Reversible description of some grammar.
-- - 'a' is the result type the Grammar shold parse/ print/ act upon.
--
-- 'Reversible' means a Grammar has access to reversible functions
-- 'rpure', 'rempty', 'ralt', 'rmap', 'rap' as well as the core Grammar defined
-- in 'GrammarInstr'.
type Grammar a = Reversible GrammarInstr a

-- | A 'Reversible' thing can be 'Label'led to describe itself, often for the
-- purpose of debugging.
label
  :: GrammarInstr :<- i
  => Label
  -> Reversible i a
  -> Reversible i a
label l gr = reversible $ GLabel l gr

-- | A single character.
charIs :: Char -> Grammar ()
charIs c =
  let txt = T.singleton c
   in label (descriptiveLabel txt) . textIs $ txt

-- | Any single character.
-- ANY including spaces, newlines, etc.
anyChar
  :: GrammarInstr :<- i
  => Reversible i Char
anyChar = reversible GAnyChar

-- | A character that matches a predicate
charWhen :: (Char -> Bool) -> Grammar Char
charWhen p = try $ predI \$/ anyChar
  where
    predI = Iso
      (\c -> if p c then Just c else Nothing)
      (\c -> if p c then Just c else Nothing)

-- | A single upper case character.
upper :: Grammar Char
upper = label (descriptiveLabel "upper") $ charWhen isUpper

-- | A single lower case character.
lower :: Grammar Char
lower = label (descriptiveLabel "lower") $ charWhen isLower

-- | A single digit character.
digit :: Grammar Char
digit = label (descriptiveLabel "digit") $ charWhen isDigit

arrow, bar, star, plus, comma, upArrow, lambda, langle, rangle, lparen, rparen, underscore, union, question, at, bigLambda, bigAt, spaceLike :: Grammar ()
arrow      = charIs '→' \|/ textIs "->"
bar        = charIs '|'
star       = charIs '*'
plus       = charIs '+'
comma      = charIs ','
upArrow    = charIs '^'
lambda     = charIs 'λ' \|/ charIs '\\'
langle     = charIs '<'
rangle     = charIs '>'
lparen     = charIs '('
rparen     = charIs ')'
underscore = charIs '_'
union      = charIs '∪' \|/ charIs 'U'
question   = charIs '?'
at         = charIs '@'
bigLambda  = charIs 'Λ' \|/ textIs "/\\"
bigAt      = charIs '#'
spaceLike  = label (descriptiveLabel "spaceLike") $ alternatives . map textIs $ [" ","\t","\n","\r"]

-- | A string of Text with try semantics.
--
-- E.G.
-- Given:
-- input = "abz"
--
-- And gramamrs:
-- charSequenceGrammar = charIs 'a' */ charIs 'b' */ charIs 'c'
-- textSequenceGrammar = textIs "abc"
--
-- Then interpreting with a backtracking parser would result in:
-- - leftovers of 'z' for charSequenceGrammar
-- - leftovers of 'abz' for textSequenceGrammar
textIs :: Text -> Grammar ()
textIs txt = label (descriptiveLabel txt) . try . textIs' $ txt

textIs' :: Text -> Grammar ()
textIs' txt = case T.uncons txt of
  Nothing
    -> rpure ()

  Just (c,cs)
    ->  inverse (elementIso ((), ()))
    \$/ (inverse (elementIso c) \$/ anyChar)
    \*/ textIs' cs

-- | Try should backtrack any notion of consumed input upon failure.
try
  :: GrammarInstr :<- i
  => Reversible i a
  -> Reversible i a
try = reversible . GTry

-- | A Grammar between parentheses.
betweenParens :: Show a => Grammar a -> Grammar a
betweenParens a = between (token lparen) a (token rparen)

-- | Longest matching text.
longestMatching :: (Char -> Bool) -> Grammar Text
longestMatching p = label (enhancingLabel "longestMatching") $ concatI \$/ rmany (charWhen p)
  where
    concatI :: Iso String Text
    concatI = Iso
      (Just . T.pack)
      (Just . T.unpack)

-- | Longest matching text. At least one character.
longestMatching1 :: (Char -> Bool) -> Grammar Text
longestMatching1 p = label (enhancingLabel "longestMatching1") $ concatI \$/ rmany1 (charWhen p)
  where
    concatI :: Iso String Text
    concatI = Iso
      (Just . T.pack)
      (Just . T.unpack)

-- | A natural number: zero and positive integers
natural :: Grammar Int
natural = label (descriptiveLabel "natural") $ naturalI \$/ longestMatching1 isDigit
  where
    naturalI :: Iso Text Int
    naturalI = Iso
      (Just . read . T.unpack) -- TODO: partial
      (Just . T.pack . show)

-- | Space is permitted to parse but none printed.
-- 0-n parsed, 0 printed.
spaceAllowed :: Grammar ()
spaceAllowed = label (descriptiveLabel "spaceAllowed") $ allowed spaceLike

-- | Space is required to parse, one is printed.
-- 1-n parsed, 1 printed.
spaceRequired :: Grammar ()
spaceRequired = label (descriptiveLabel "spaceRequired") $ required spaceLike

-- | Space preferred to parse, one is printed.
-- 0-n parsed, 1 printed.
spacePreferred :: Grammar ()
spacePreferred = label (descriptiveLabel "spacePreferred") $ ignoreIso [()] \$/ rmany spaceLike

-- | Separate at least one Grammar by a separator.
-- E.G.
--   sepBy1 (textIs ',') digit
-- would parse:
--  "1"
--  "1,2"
--  "1,2,3"
sepBy1
  :: (Show a, Eq a)
  => Grammar ()
  -> Grammar a
  -> Grammar (NonEmpty a)
sepBy1 sepG g = iso \$/ sepBy1' sepG g
  where
    iso :: Iso [a] (NonEmpty a)
    iso = Iso
      {_forwards = \case
         []
           -> Nothing
         (a:as)
           -> Just $ a NE.:| as
      ,_backwards = \ne -> Just $ NE.toList ne
      }

-- Implementation of sepBy1 that uses a weaker list type rather than a NonEmpty
sepBy1'
  :: (Show a, Eq a)
  => Grammar ()
  -> Grammar a
  -> Grammar [a]
sepBy1' sepG g = iso \$/ g \*/ alternatives [ try $ sepG */ sepBy1' sepG g
                                            , rpure []
                                            ]
  where
    iso :: Iso (a,[a]) [a]
    iso = Iso
      { _forwards  = \(a,as) -> Just $ a:as
      , _backwards = \case
        a:as
          -> Just (a,as)
        _ -> Nothing
      }

-- | Separate 0 or many Grammars by a separator.
-- E.G.
--   sepBy (textIs ',') digig
-- would parse nothing or:
-- ""
-- "1"
-- "1,2"
-- "1,2,3"
sepBy
  :: (Show a, Eq a)
  => Grammar ()
  -> Grammar a
  -> Grammar [a]
sepBy sepG g = alternatives [ try $ sepBy1' sepG g
                            , rpure []
                            ]


-- | A permissive grammar is itself and also may have:
-- - Any number of spaces preceeding
-- - Any number of enclosing (matched) parentheses
--  - Which themselves may have internal spacing.
permissive
  :: Show a
  => Grammar a
  -> Grammar a
permissive a =
  token a \|/ betweenMany1 (token lparen) (token a) (token rparen)

-- | A token allows 0-n preceeding spaces
token
  :: Show a
  => Grammar a
  -> Grammar a
token gr = spaceAllowed */ gr

{- TODO: These functions should be replaced with a chain or removed -}

tokenThenMany1ThenSomething
  :: ( Eq xs
     , Show xs
     , Show r
     , Show a
     )
  => Grammar ()
  -> Grammar xs
  -> Grammar a
  -> Iso ([xs],a) r
  -> Grammar r
tokenThenMany1ThenSomething t many something iso
  = t
  */ (iso \$/ rmany1 ((betweenParens many \|/ many) \* spaceRequired)
          \*/ (betweenParens something \|/ something)
     )

