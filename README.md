# PLGrammar - experimental

This package describes a Grammar Instruction using [DSL-Compose](https://github.com/syallop/DSL-Compose).
The intent is that it can be used as either a parser or a printer depending on 
context. This could prevent round-trip properties from being accidentally
violated for example, by adjusting a parser but not the printer.

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
| `spacePrefered :: Grammar ()`                       | `*` spaces parsed, `1` printed       |

