# PLGrammar - experimental

This package can be used to define `Grammar`s which are descriptions of a type
both 'forwards' and 'backwards'.

For example, 'forwards' could correspond to a Parser which turns Text into the
value whereas 'backwards' could correspond to a Printer which would print the
value back to Text.

The main benefit of describing both directions at once is that you can avoid
violating roundtrip properties that might otherwise occur when subtle
differences are introduced. 

If you derive parsers and printers from the same grammar you: 
- Cannot forget to add a feature to one side
- Explicitly highlight differences in direction
  - E.G. if unicode characters are accepted in parse but printed in ascii you
    will have a grammar combinator that declares this rather than the logic
    being hidden in two implementations.

Note:
- This package has spun off a programming language project [PL](https://github.com/syallop/PL) where it is used to define a [lispy syntax](https://github.com/syallop/PLLispy) which compiles to a [printer](https://github.com/syallop/PLPrinter) and [parser](https://github.com/syallop/PLParser) as used in a [repl](https://github.com/syallop/PLRepl).
- Testing/ quality is currently minimal

## Contents
- [Example problem](#Example-Problem)
- [Reversibility](#Reversibility)
- [Core operations](#Core-operations)
  - [Labels](#Labels)
- [Convenience functions](#Convenience-functions)

## Example Problem

**Note**:
- Working definitions for this example can be found in the test suite `Test/PLGrammar/TestSpec.hs`
- Several shortcuts have been taken

Problem: Read a sequence of values and then write them back.

Complication: The values are written by a human. Therefore they may have
irregular spacing, inconsistent quoting of strings and ridiculous
capitalisation.

Aim:
- Be permissive with the input we accept
- Output in a regular format
- But retain any useful formatting internally. E.G. The capitalisation of words

At the end we will have a single top level `Grammar` definition that can:
- Parse strings like:
  ```
  abc,"Abc" ,"aBc", 1 , ABC,abc     ,     1.
  ```
- And print the string:
  ```
  "abc", "abc", "abc", 1, "abc", "abc", 1.
  ```

Some definitions
```haskell
-- Values are a single character or a string of text 
data Value
  = CharValue Char
  | TextValue Text
  deriving (Show, Eq)

-- Sequences have at least one value
data Sequence
  = End Value
  | Seq Value Sequence
  deriving (Show, Eq)
```

Grammars for values:
```haskell
-- - Single integers accepted going forward
-- - Single integers returned going backwards
charValue :: Grammar Value
charValue = textIs "1" */ (rpure $ CharValue '1')

-- - Optionally quoted strings accepted going forward
-- - Strings quoted going backward
textValue :: Grammar Value
textValue = (preferQuotes $ textIs "abc" */ (rpure $ TextValue "abc"))

preferQuotes :: (Eq a, Show a) => Grammar a -> Grammar a
preferQuotes g = alternatives
  [ try $ textIs "\"" */ g \* textIs "\""
  , g
  ]

value :: Grammar Value
value = alternatives [charValue, textValue]
```
Some notes:
- We've taken shortcuts. The only characters we accept are '1'. The only text we
  accept is 'abc' or '"abc"'. A full example would use character classes here.
- We use 'rpure' instead of 'pure'. This requires an `Eq,Show` constraint and
  allows the Grammar to be ran in reverse.
  I.E. Ran forward (parsing) we always return the value.
       Ran backwards (printing) we need to know if the Grammar matches by
       checking whether the values are equal.

- `alternatives` can also be written like `g0 <|> g1 ... <|> gn` and means that,
  left-to-right the constituent grammars are attempted.
- The 'try' combinator allows the following Grammar to fail even if the first
  component have 'matched' input.
  E.G. If an opening quote is matched but a closing quote is not found, instead
  of failing entirely with something like "expected to find closing quote" we
  may wish to pretend the parse never started so that we can try another.


A full definition of `Printers` and `Parsers` can be found in the tests. Their
types look like:
```haskell
-- Parse Text with leftovers
type Parser a = Text -> (Text, Maybe a)

-- Attempt to print a value to Text
type Printer a = a -> Maybe Text
```

We need functions to convert to our forwards(parser) and backwards(printer).
These are mechanical. See the code for definitions.
```haskell
toParser :: Grammar a -> Parser a
toPrinter :: Grammar a -> Printer a
```

We should then be able to test parsing and printing input and see:

| Input | print . parse | 
| ----  | -----         |
| `1`   | `1`           |
| `abc` | `"abc"`       |
| `"abc"` | `"abc"` |

Note that quotes are not required when parsing but will always be added when
printing due to the use of the `preferQuotes` combinator. 

We can now define the Grammar for sequences such that they:
- Require commas between items
  - Allow spaces before commas when parsing but ignore when printing
  - Prefer a single space after commas

```haskell
sequence :: Grammar Sequence
sequence = alternatives
  [ try $ sequenceEnd
  , sequenceSeq
  ]

sequenceEnd :: Grammar Sequence
sequenceEnd = endIso \$/ (value \* textIs ".")

sequenceSeq :: Grammar Sequence
sequenceSeq = seqIso \$/ (value \* spacedComma) \*/ sequence

spacedComma :: Grammar ()
spacedComma = allowed many1Spaces */ textIs "," \* preferred many1Spaces
  where
    many1Spaces = ignoreIso [()] \$/ (rmany1 $ textIs " ")

endIso :: Iso Value Sequence
seqIso :: Iso (Value,Sequence) Sequence
```

Note:
- `\*/` is a reversible variant of `<*>` which must use tuples `Grammar (a,b)` rather
  than functions `Grammar (a -> b)`.
- `\$/` is a reversible variant of `<$>` which must use a reversible function
  `Iso` rather than a plain function.
- `seqIso` is a reversible function which understands how to both extract the `Seq`
  case of `Sequence` and construct `Sequences` from the data in a `Seq`. These
  are fairly mechanical to write for constructors. The could be generated by
  templateHaskell. They take the form:
  ```haskell
    seqIso :: Iso (Value,Sequence) Sequence
    seqIso = Iso
      { _forwards  = \(v,s) -> Just $ Seq v s
      , _backwards = \s -> case s of
          Seq v s1
            -> Just (v,s1)
          _ -> Nothing
      }
  ```
- `rmany1` is the reversible variant of `many1` and accepts one or many of a
  grammar.
- `ignoreIso [()]` discards the result of the `rmany1` such that `many1Spaces`
  returns a `Grammar ()`.

We should then be able to test parsing and printing input and see:

```
Parse: abc,"abc" ,"abc", 1 , abc,abc     ,     1.
Print: "abc", "abc", "abc", 1, "abc", "abc", 1. 
```


## Reversibility
Grammar is defined as `type Grammar a = Reversible GrammarInstr a`.
The reversability of mapping, alternative and sequencing functions is therefore
provided by [Reversible](https://github.com/syallop/Reversible) which provides
reversible varients of common operations. In short:

| Regular function                      | Reversible Grammar equivalent                    |
| ------------------------------------- | ------------------------------------------------ |
| `<$>, fmap :: (a -> b) -> f a -> f b` | `\$/, rmap :: Iso a b -> Grammar a -> Grammar b` |
| `<\|> :: f a -> f a -> f a`           | `\\|/ :: Grammar a -> Grammar a -> Grammar a`    |
| `<*> :: f (a -> b) -> f a -> f b`     | `\*/ :: Grammar a -> Grammar b -> Grammar (a,b)` |
| `*> :: f b -> f a -> f a`             | `*/ :: Grammar () -> Grammar a -> Grammar a`     |
| `<* :: f a -> f b -> f a`             | `\* :: Grammar a -> Grammar () -> Grammar a`     |
| `empty :: f a`                        | `rempty :: Grammar a`                            |
| `pure :: a -> f a`                    | `rpure :: Eq a => a -> Grammar a`                |


## Core operations
The core `GrammarInstr` provides `anyChar`, `try` and `label`. `anyChar`
operates on any single character including spaces, newlines, etc. `try` should
backtrack any notion of consumed input upon failure. 

`labels` and convenience functions are described below.

### Labels
`label` allows Grammars to be described with a human readable label, often for
the purpose of debugging. Labels are either `Descriptive` and fully describe
the associated Grammar, or they are `Enhancing` and add additional information.

Note: These semantics are not enforced. It is currently the responsibility of
the user to decide how to interpret labels. In the future common description
strategies may be defined here or in the [PLLabel](https://github.com/syallop/PLLabel) dependency.

#### Intended label description semantics
A Grammar should be able to be described by collecting Enhancing labels and
stopping at the first Descriptive label. This means higher level descriptions
can be rendered when appropriate rather than leaking labels that form part of
the implementation details.
E.G. This allows seeing `"word"` rather than `"many1 anyChar \* charIs ' '"`.

Enhancing labels can be used to reveal implementation details that are useful
without indicating it is sufficient for such a description to stop.
Higher-order Grammar functions should often use an Enhancing label at the top
level to describe the function itself. A 'Descriptive' label is not appropriate
as the Grammar that was passed to the function likely contains it's own labels
neccessary for context.
E.G. This allows seeing `"many words"` rather than `"many"`.

## Convenience functions
The reversible instructions in combination with the 'core' operations of
'anyChar', 'try' and 'label' are sufficient for defining many of the combinators
you would expect to find in a parsing library. Some of these combinators are
defined for convenience. An selection of these functions are provided below as
an example of what is possible, rather than being an exhaustive (and correct..)
description of the API.

| Function                                            | Use as a Parser/Pretty-Printer       |
| --------------------------------------------------- | ------------------------------------ |
| Character and Text:                                 |                                      |
| `charIs :: Char -> Grammar ()`                      | Match a given character only         |
| `charWhen :: (Char -> Bool) -> Grammar Char`        | A character that matches a predicate |
| `upper, lower, diget :: Grammar Char`               | Match classes of character           |
| `space, arrow, lparen :: Grammar Char`              | Match various tokens                 |
| `textIs :: Text -> Grammar ()`                      | Match a string of Text               |
| Combinators:                                        |                                      |
| `betweenParens :: Grammar a -> Grammar a`           | A Grammar between parentheses        |
| `longestMatching :: (Char -> Bool) -> Grammar Text` | Longest matching text                |
| Differences in direction:                           |                                      |
| `spaceAllowed :: Grammar ()`                        | `*` spaces parsed, `0` printed       |
| `spaceRequired :: Grammar ()`                       | `+` spaces parsed, `1` printed       |
| `spacePreferred :: Grammar ()`                      | `*` spaces parsed, `1` printed       |

