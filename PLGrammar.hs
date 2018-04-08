{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE
    GADTs
  , RankNTypes
  , OverloadedStrings
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

Note: WIP. Currently only suitable for translation to a Parser (and maybe not
even that). Some constructors may be missing/ some may be unneccessary and in
general the entire type is likely to change.

Correspondance to invertable-syntax:
  - <$>: GIsoMap:     \$/
  - <*>: GProductMap: \*/
  - <|>: GAlt:        \|/
  - empty: GEmpty
  - pure: GPure
  - token: GAnyChar:  anyChar
-}
module PLGrammar
  ( Grammar(..)

  , charIs
  , textIs

  , anyChar
  , charWhen

  , try

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

  , seqR
  , seqL
  , (\$/)
  , (\*/)
  , (\|/)
  , (\*)
  , (*/)
  , grammarMany
  , grammarMany1

  , spaceAllowed
  , spaceRequired
  , spacePrefered

  , tokenThenMany1ThenSomething
  )
  where

import Data.Text (Text)
import Data.Char
import qualified Data.Text as T
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad

import PLGrammar.Iso
import PLLabel
import Prelude hiding ((.),id)
import Control.Category

-- | The grammar of some language.
data Grammar a where
  -- Any character
  GAnyChar
    :: Grammar Char

  -- Applicative-like pure
  GPure
    :: Eq a
    => a
    -> Grammar a

 -- Alternative empty
  GEmpty
    :: Grammar a

  -- Alternative <|>
  GAlt
    :: Grammar a
    -> Grammar a
    -> Grammar a

  GIsoMap
    :: Show a
    => Iso a b
    -> Grammar a
    -> Grammar b

  GProductMap
    :: (Show a, Show b)
    => Grammar a
    -> Grammar b
    -> Grammar (a,b)

  GLabel
    :: Label
    -> Grammar a
    -> Grammar a

  GTry
    :: Grammar a
    -> Grammar a

-- Takes () on discarded result unlike Applicative
(*/), seqR :: Show a => Grammar () -> Grammar a -> Grammar a
(*/) = seqR
seqR g0 g1 = inverseIso unitI . flipI \$/ g0 \*/ g1


(\*), seqL :: Show a => Grammar a -> Grammar () -> Grammar a
(\*) = seqL
seqL g0 g1 = inverseIso unitI \$/ g0 \*/ g1

-- | A single character.
charIs :: Char -> Grammar ()
charIs c =
  let txt = T.singleton c
   in GLabel (descriptiveLabel txt) . textIs $ txt

-- | Any single character.
-- ANY including spaces, newlines, etc.
anyChar :: Grammar Char
anyChar = GAnyChar

-- | A character that matches a predicate
charWhen :: (Char -> Bool) -> Grammar Char
charWhen p = GTry $ predI \$/ anyChar
  where
    predI = Iso
      ["some character predicate"]
      (\c -> if p c then Just c else Nothing)
      (\c -> if p c then Just c else Nothing)

upper :: Grammar Char
upper = GLabel (descriptiveLabel "upper") $ charWhen isUpper

lower :: Grammar Char
lower = GLabel (descriptiveLabel "lower") $ charWhen isLower

digit :: Grammar Char
digit = GLabel (descriptiveLabel "digit") $ charWhen isDigit

arrow      = charIs '→' -- \|/ textIs "->"
bar        = charIs '|'
star       = charIs '*'
plus       = charIs '+'
comma      = charIs ','
upArrow    = charIs '^'
lambda     = charIs '\\' \|/ charIs 'λ'
langle     = charIs '<'
rangle     = charIs '>'
lparen     = charIs '('
rparen     = charIs ')'
underscore = charIs '_'
union      = charIs '∪' \|/ charIs 'U'
question   = charIs '?'
at         = charIs '@'
bigLambda  = textIs "/\\" \|/ charIs 'Λ'
bigAt      = charIs '#'
spaceLike  = GLabel (descriptiveLabel "spaceLike") $ alternatives . map textIs $ [" ","\t","\n","\r"]

-- | A string of Text
textIs :: Text -> Grammar ()
textIs txt = GLabel (descriptiveLabel txt) . GTry . textIs' $ txt
  where
    textIs' txt = case T.uncons txt of
      Nothing
        -> GPure ()

      Just (c,cs)
        ->  inverseIso (elementIso ((), ()))
        \$/ (inverseIso (elementIso c) \$/ anyChar)
        \*/ textIs' cs

try :: Grammar a -> Grammar a
try = GTry

-- | A Grammar between two others.
between :: Show a => Grammar () -> Grammar a -> Grammar () -> Grammar a
between l a r = l */ a \* r

-- | A Grammar between parentheses.
betweenParens :: Show a => Grammar a -> Grammar a
betweenParens a = between lparen a rparen

-- | Longest matching text.
longestMatching :: (Char -> Bool) -> Grammar Text
longestMatching p = GLabel (enhancingLabel "longestMatching") $ concatI \$/ grammarMany (charWhen p)
  where
    concatI :: Iso String Text
    concatI = Iso
      ["longestMatching"]
      (Just . T.pack)
      (Just . T.unpack)

-- | Longest matching text. At least one character.
longestMatching1 :: (Char -> Bool) -> Grammar Text
longestMatching1 p = GLabel (enhancingLabel "longestMatching1") $ concatI \$/ grammarMany1 (charWhen p)
  where
    concatI :: Iso String Text
    concatI = Iso
      ["longestMatching1"]
      (Just . T.pack)
      (Just . T.unpack)

-- | A natural number: zero and positive integers
natural :: Grammar Int
natural = GLabel (descriptiveLabel "natural") $ naturalI \$/ longestMatching1 isDigit
  where
    naturalI :: Iso Text Int
    naturalI = Iso
      ["natural"]
      (Just . read . T.unpack) -- TODO: partial
      (Just . T.pack . show)

-- | A list of alternative Grammars.
alternatives :: [Grammar a] -> Grammar a
alternatives = foldr GAlt GEmpty

grammarMany :: (Eq a,Show a) => Grammar a -> Grammar [a]
grammarMany g = GLabel (enhancingLabel "grammarMany") $ grammarMany1 g \|/ GPure []

grammarMany1 :: (Eq a,Show a) => Grammar a -> Grammar [a]
grammarMany1 g = GLabel (enhancingLabel "grammarMany1") $ consI \$/ g \*/ grammarMany g

infixl 3 \|/
infixr 6 \*/
infix 5 \$/

(\|/) = GAlt

(\*/) :: (Show a,Show b) => Grammar a -> Grammar b -> Grammar (a,b)
(\*/) = GProductMap

(\$/) :: (Show a,Show b) => Iso a b -> Grammar a -> Grammar b
(\$/) = GIsoMap

-- | A grammar is permitted to parse but not printed.
allowed :: Grammar () -> Grammar ()
allowed g = ignoreIso [] \$/ grammarMany g

-- | A grammar is required to parse, one is printed.
required :: Grammar () -> Grammar ()
required g = g \* allowed g

-- | A grammar is required to parse, one is printed.
prefered :: Grammar () -> Grammar ()
prefered g = ignoreIso [()] \$/ grammarMany g

-- | Space is permitted to parse but none printed.
spaceAllowed :: Grammar ()
spaceAllowed = GLabel (descriptiveLabel "spaceAllowed") $ allowed spaceLike

-- | Space is required to parse, one is printed.
spaceRequired :: Grammar ()
spaceRequired = GLabel (descriptiveLabel "spaceRequired") $ required spaceLike

-- | Space prefered to parse, one is printed.
spacePrefered :: Grammar ()
spacePrefered = GLabel (descriptiveLabel "spacePrefered") $ ignoreIso [()] \$/ grammarMany spaceLike

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
tokenThenMany1ThenSomething token many something iso
  = token
  */ (iso \$/ grammarMany1 ((betweenParens many \|/ many) \* spaceRequired)
          \*/ (betweenParens something \|/ something)
     )

combiner :: (a -> x -> x) -> [a] -> x -> x
combiner f []     _ = error "Cant combine empty list"
combiner f [a]    x = f a x
combiner f (a:as) x = f a $ combiner f as x

