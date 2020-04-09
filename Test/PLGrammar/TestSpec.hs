{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module PLGrammar.TestSpec where

import Test.Hspec

import Data.Text
import qualified Data.Text as Text
import Control.Monad
import Data.Monoid
import Data.Semigroup
import Control.Applicative

import Reversible
import Reversible.Iso
import PLGrammar

data TestCase a = TestCase
  { _testCase             :: Text
  , _input                :: Text
  , _grammar              :: Grammar a
  , _shouldParse          :: Maybe a
  , _shouldParseLeftovers :: Text
  , _shouldPrint          :: Maybe Text
  }

testcase :: (Show a, Eq a) => TestCase a -> Spec
testcase (TestCase name input grammar shouldParse shouldParseLeftovers shouldPrint) = describe (Text.unpack name) $ do
  let parser  = toParser  grammar
      printer = toPrinter grammar

      (leftovers,parse) = runParser parser input

  it "parse leftovers" $ leftovers `shouldBe` shouldParseLeftovers
  it "parse result"    $ parse     `shouldBe` shouldParse

  case shouldParse of
    -- If there is no value theres nothing to print?
    Nothing
      -> pure ()
    Just v
      -> it "print result" $ runPrinter printer v `shouldBe` shouldPrint

spec :: Spec
spec = do
  -- Establish the most simple case works
  testcase $ TestCase
    { _testCase             = "Simple, single chars: 1 -> 1"
    , _input                = "1"
    , _grammar              = value
    , _shouldParse          = Just $ CharValue '1'
    , _shouldParseLeftovers = ""
    , _shouldPrint          = Just "1"
    }

  -- Can optional things be omitted going forward and required going backward?
  testcase $ TestCase
    { _testCase             = "Strings without optional fragments: abc -> \"abc\""
    , _input                = "abc"
    , _grammar              = value
    , _shouldParse          = Just $ TextValue "abc"
    , _shouldParseLeftovers = ""
    , _shouldPrint          = Just "\"abc\""
    }

  -- Are optional things be provided going forward and required going backward?
  testcase $ TestCase
    { _testCase             = "Strings with optional fragments: \"abc\" -> \"abc\""
    , _input                = "\"abc\""
    , _grammar              = value
    , _shouldParse          = Just $ TextValue "abc"
    , _shouldParseLeftovers = ""
    , _shouldPrint          = Just "\"abc\""
    }

data Sequence
  = End Value
  | Seq Value Sequence
  deriving (Show, Eq)

data Value
  = CharValue Char
  | TextValue Text
  deriving (Show, Eq)

-- Intended semantics:
-- - Single integers accepted going forward
-- - Single integers returned going backwards
charValue :: Grammar Value
charValue = textIs "1" */ (rpure $ CharValue '1') -- TODO: Read full type

-- Intended semantics:
-- - Optionally quoted strings accepted going forward
-- - Strings quoted going backward
textValue :: Grammar Value
textValue = preferQuotes $ textIs "abc" */ (rpure $ TextValue "abc") -- TODO: Read full type

-- TODO: Can this be written in terms of a prefer combinator? (Possibly not as
-- the first quote matching requires the last to match)
preferQuotes :: (Eq a, Show a) => Grammar a -> Grammar a
preferQuotes g = alternatives
  [ try $ textIs "\"" */ g \* textIs "\""
  , g
  ]

value :: Grammar Value
value = alternatives [charValue, textValue]

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
