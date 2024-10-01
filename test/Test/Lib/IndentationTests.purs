module Test.Lib.IndentationTests where

import Parsing.Indent (IndentParser, checkIndent, indentParens, indented, runIndent, withPos)
import Prelude (class Eq, class Show, Unit, bind, discard, pure, void, ($), (*>), (<$>), (<*), (<<<))
import Test.Lib (class ParseErrorHuman__OnlyString, TestM, mkParseErrorTestMessage, mkParseErrorTestPosition, mkParseErrorTestPositionAndMessage, mkParseTest)

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Parsing (Position(..), runParserT)
import Parsing.Combinators (many, sepBy)
import Parsing.Combinators.Array as Combinators.Array
import Parsing.Indent as Indent
import Parsing.String (char, eof, string)
import Parsing.String.Basic (alphaNum, intDecimal, skipSpaces)

parseTest :: forall s a. Show a => Eq a => ParseErrorHuman__OnlyString s => s -> a -> IndentParser s a -> Effect Unit
parseTest = mkParseTest (\input -> runIndent <<< runParserT input)

parseErrorTestPosition :: forall s a. Show a => IndentParser s a -> s -> Position -> Effect Unit
parseErrorTestPosition = mkParseErrorTestPosition (\input -> runIndent <<< runParserT input)

parseErrorTestMessage :: forall s a. Show a => IndentParser s a -> s -> String -> Effect Unit
parseErrorTestMessage = mkParseErrorTestMessage (\input -> runIndent <<< runParserT input)

parseErrorTestPositionAndMessage :: forall s a. Show a => IndentParser s a -> s -> String -> Position -> Effect Unit
parseErrorTestPositionAndMessage = mkParseErrorTestPositionAndMessage (\input -> runIndent <<< runParserT input)
