# Notes

## Potential problems

## Under which section to put new Reporting.Annotation.New module?

Currently in other modules, should it be somewhere else?

### Changing behavior of adapter module changes behavior of unit tests

### Tests that relied on legacy parsec functions now use new ones.

Previously `./elm-format-lib/test/Parse/TestHelpers.hs` had this import:
`import Text.ParserCombinators.Parsec.Combinator (eof)`
which have now been replace with:
`import Text.Parsec.Adapter (eof)`

And `./elm-format-lib/test/Parse/HelpersTest.hs` had this:
`import Text.ParserCombinators.Parsec.Char (lower)`
which have now been replaced with:
`import Text.Parsec.Adapter (lower)`
