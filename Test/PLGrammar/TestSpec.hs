{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module PLGrammar.TestSpec where

import Test.Hspec

import Prelude hiding (sequence)

import Data.Text
import qualified Data.Text as Text
import Control.Monad hiding (sequence)
import Data.Monoid
import Data.Semigroup
import Control.Applicative

import Reversible
import Reversible.Iso
import PLGrammar

data TestCase a = TestCase
  { _testCase             :: Text
  , _input                :: [Text]
  , _grammar              :: Grammar a
  , _shouldParse          :: Maybe a
  , _shouldParseLeftovers :: Text
  , _shouldPrint          :: Maybe Text
  }

testcase :: (Show a, Eq a) => TestCase a -> Spec
testcase (TestCase name inputs grammar shouldParse shouldParseLeftovers shouldPrint) = describe (Text.unpack name) $ do
  let parser  = toParser  grammar
      printer = toPrinter grammar

  describe "All inputs should parse to the expected value" $ do
    forM_ inputs $ \input -> testParse input parser (shouldParseLeftovers, shouldParse)

  describe "Value should print as expected" $ testPrint shouldParse printer shouldPrint

  describe "Printed text should parse back to ensure roundtrip properties" $ case shouldPrint of
    Nothing
      -> pure ()
    Just p
      -> testParse p parser (shouldParseLeftovers, shouldParse)

testParse :: (Show a, Eq a) => Text -> Parser a -> (Text,Maybe a) -> Spec
testParse input parser (shouldParseLeftovers, shouldParse) = do
  let (leftovers, mResult) = runParser parser input
  it "has correct leftovers" $ leftovers `shouldBe` shouldParseLeftovers
  it "has correct result" $ mResult `shouldBe` shouldParse

testPrint :: Maybe a -> Printer a -> Maybe Text -> Spec
testPrint input printer shouldPrint = case input of
    -- If there is no value theres nothing to print?
    Nothing
      -> pure ()
    Just v
      -> it "print result" $ (runPrinter printer v) `shouldBe` shouldPrint

spec :: Spec
spec = do
  describe "Sequence example" $ do
    testcase $ TestCase
      { _testCase             = "Single chars"
      , _input                = ["1"]
      , _grammar              = value
      , _shouldParse          = Just $ CharValue '1'
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "1"
      }
    testcase $ TestCase
      { _testCase             = "Strings"
      , _input                = ["abc"
                                ,"\"abc\""
                                ]
      , _grammar              = value
      , _shouldParse          = Just $ TextValue "abc"
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "\"abc\""
      }

    -- Singleton sequences
    testcase $ TestCase
      { _testCase             = "Singleton sequence of char"
      , _input                = ["1."]
      , _grammar              = sequence
      , _shouldParse          = Just $ End (CharValue '1')
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "1."
      }
    testcase $ TestCase
      { _testCase             = "Singleton sequence of text"
      , _input                = ["abc."
                                ,"\"abc\"."
                                ]
      , _grammar              = sequence
      , _shouldParse          = Just $ End (TextValue "abc")
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "\"abc\"."
      }

    -- Longer sequences
    testcase $ TestCase
      { _testCase             = "Longer sequence of chars"
      , _input                = ["1,1."]
      , _grammar              = sequence
      , _shouldParse          = Just $ Seq (CharValue '1') $ End (CharValue '1')
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "1, 1."
      }
    testcase $ TestCase
      { _testCase             = "Longer sequence of values with optionals at end"
      , _input                = ["1,abc."
                                ,"1,\"abc\"."
                                ]
      , _grammar              = sequence
      , _shouldParse          = Just $ Seq (CharValue '1') $ End (TextValue "abc")
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "1, \"abc\"."
      }
    testcase $ TestCase
      { _testCase             = "Longer sequence of values with optionals at start"
      , _input                = ["abc,1."
                                ,"\"abc\",1."
                                ]
      , _grammar              = sequence
      , _shouldParse          = Just $ Seq (TextValue "abc") $ End (CharValue '1')
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "\"abc\", 1."
      }

    -- Test spacing rules
    testcase $ TestCase
      { _testCase             = "Test trailing and preceeding spaces"
      , _input                = ["abc,1."
                                ,"abc, 1."
                                ,"abc ,1."
                                ,"abc , 1."
                                ,"abc   ,1."
                                ,"abc,   1."
                                ]
      , _grammar              = sequence
      , _shouldParse          = Just $ Seq (TextValue "abc") $ End (CharValue '1')
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "\"abc\", 1."
      }
    testcase $ TestCase
      { _testCase             = "Test a sequence with various combinations of preceeding and trailing spaces and optional quotes"
      , _input                = ["abc,\"abc\" ,\"abc\", 1 , abc,abc     ,     1."]
      , _grammar              = sequence
      , _shouldParse          = Just $ Seq (TextValue "abc") $ Seq (TextValue "abc") $ Seq (TextValue "abc") $ Seq (CharValue '1') $ Seq (TextValue "abc") $ Seq (TextValue "abc") $ End (CharValue '1')
      , _shouldParseLeftovers = ""
      , _shouldPrint          = Just "\"abc\", \"abc\", \"abc\", 1, \"abc\", \"abc\", 1."
      }

data Value
  = CharValue Char
  | TextValue Text
  deriving (Show, Eq)

charIso :: Iso Char Value
charIso = Iso
  { _forwards  = Just . CharValue
  , _backwards = \v -> case v of
      CharValue c
        -> Just c
      _ -> Nothing
  }

textIso :: Iso Text Value
textIso = Iso
  { _forwards  = Just . TextValue
  , _backwards = \v -> case v of
      TextValue t
        -> Just t
      _ -> Nothing
  }

-- Intended semantics:
-- - Single integers accepted going forward
-- - Single integers returned going backwards
charValue :: Grammar Value
charValue = charIso \$/ textIs "1" */ rpure '1' -- TODO: Read full type

-- Intended semantics:
-- - Optionally quoted strings accepted going forward
-- - Strings quoted going backward
textValue :: Grammar Value
textValue = textIso \$/ (preferQuotes $ textIs "abc" */ rpure "abc") -- TODO: Read full type

-- TODO: Can this be written in terms of a prefer combinator? (Possibly not as
-- the first quote matching requires the last to match)
preferQuotes :: (Eq a, Show a) => Grammar a -> Grammar a
preferQuotes g = alternatives
  [ try $ textIs "\"" */ g \* textIs "\""
  , g
  ]

-- Intended semantics:
-- - Either a char or a text value is accepted.
value :: Grammar Value
value = alternatives [charValue, textValue]

data Sequence
  = End Value
  | Seq Value Sequence
  deriving (Show, Eq)

endIso :: Iso Value Sequence
endIso = Iso
  { _forwards  = Just . End
  , _backwards = \s -> case s of
      End v
        -> Just v
      _ -> Nothing
  }

seqIso :: Iso (Value,Sequence) Sequence
seqIso = Iso
  { _forwards  = \(v,s) -> Just $ Seq v s
  , _backwards = \s -> case s of
      Seq v s1
        -> Just (v,s1)
      _ -> Nothing
  }

-- Intended semantics:
-- - sequence separated by commas
-- - sequence terminated by a period
-- - Spaces before commas should be allowed but ignored in reverse
-- - Spaces after commas should be allowed but become one in reverse
sequence :: Grammar Sequence
sequence = alternatives
  [ try $ sequenceEnd
  , sequenceSeq
  ]

sequenceEnd :: Grammar Sequence
sequenceEnd = endIso \$/ (value \* textIs ".")

sequenceSeq :: Grammar Sequence
sequenceSeq = seqIso \$/ (value \* spacedComma) \*/ sequence

-- A comma with spacing rules:
-- - Allow preceeding space forwards, ignore all backwards
-- - Prefer a trailing space forwards, use a single space backwards
spacedComma :: Grammar ()
spacedComma = allowed many1Spaces */ textIs "," \* preferred many1Spaces
  where
    many1Spaces = ignoreIso [()] \$/ (rmany1 $ textIs " ")

-- A Parser reads Text into some result type and has the possibility of
-- producing leftover text.
--
-- Parsers may be ran in sequence with >> and used as alternatives with <|>.
--
-- Construct Parser from Grammars with toParser
newtype Parser a = Parser (Text -> (Text, Maybe a))

instance Semigroup a => Semigroup (Parser a) where
  pa0 <> pa1 = do
    a0 <- pa0
    a1 <- pa1
    pure (a0 <> a1)

instance Monoid a => Monoid (Parser a) where
  mempty = return mempty

instance Functor Parser where
  fmap f (Parser pa) = Parser $ \txt -> case pa txt of
    (leftovers,Just a)
      -> (leftovers,Just $ f a)

    (leftovers,Nothing)
      -> (leftovers,Nothing)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \txt -> (txt, Just a)

  (Parser pa) >>= f = Parser $ \txt -> case pa txt of
    (leftovers, Just a)
      -> let Parser pb = f a
          in pb leftovers

    (leftovers, Nothing)
      -> (leftovers, Nothing)

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = Parser $ \txt -> (txt,Nothing)

  mplus (Parser pa0) (Parser pa1) = Parser $ \txt0 -> case pa0 txt0 of
    (leftovers,Just a)
      -> (leftovers,Just a)

    (leftovers,Nothing)
      -> if txt0 == leftovers
           then pa1 txt0
           else (leftovers,Nothing)

toParser :: Grammar a -> Parser a
toParser (Reversible g) = case g of
  ReversibleInstr i
    -> case i of
         GAnyChar
           -> Parser $ \txt -> case Text.uncons txt of
                Nothing
                  -> (txt,Nothing)
                Just (c,txt')
                  -> (txt',Just c)

         GLabel _ g
           -> toParser g

         GTry g
           -> Parser $ \txt ->  case runParser (toParser g) txt of
                (leftovers,Nothing)
                  -> (txt,Nothing)
                success
                  -> success
  RPure a
    -> Parser $ \txt -> (txt, Just a)

  REmpty
    -> Parser $ \txt -> (txt, Nothing)

  RAlt g0 g1
    -> toParser g0 <|> toParser g1

  RMap iso ga
    -> let Parser p = toParser ga
        in Parser $ \txt -> case p txt of
             (leftovers,Nothing)
               -> (leftovers,Nothing)

             (leftovers,Just res)
               -> case forwards iso res of
                    Nothing
                      -> (leftovers,Nothing)
                    Just b
                      -> (leftovers,Just b)

  RAp ga gb
    -> (,) <$> toParser ga <*> toParser gb

runParser :: Parser a -> Text -> (Text,Maybe a)
runParser (Parser p) txt = p txt

newtype Printer a = Printer (a -> Maybe Text)

toPrinter :: Grammar a -> Printer a
toPrinter (Reversible g) = case g of
  ReversibleInstr i
    -> case i of
         GAnyChar
           -> Printer $ \a -> Just $ Text.singleton a

         GLabel _ g
           -> toPrinter g

         GTry g
           -> toPrinter g

  RPure a
    -> Printer $ \a' -> if a == a' then Just mempty else Nothing

  REmpty
    -> Printer . const $ Nothing

  RAlt g0 g1
    -> Printer $ \a -> let Printer p = toPrinter g0
                           Printer q = toPrinter g1
                        in mplus (p a) (q a)

  RMap iso ga
    -> Printer $ let Printer p = toPrinter ga
                  in backwards iso >=> p

  RAp ga gb
    -> Printer $ let Printer p = toPrinter ga
                     Printer q = toPrinter gb
                  in \(a,b) -> liftM2 mappend (p a) (q b)

runPrinter :: Printer a -> a -> Maybe Text
runPrinter (Printer f) a = f a
