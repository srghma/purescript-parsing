-- Run tests:
--
--     spago -x spago-dev.dhall test
--

module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix, defer)
import Control.Monad.State (State, lift, modify, runState)
import Data.Array (some, toUnfoldable)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.CodePoint.Unicode as CodePoint.Unicode
import Data.Either (Either(..), either, fromLeft, hush)
import Data.Foldable (oneOf)
import Data.List (List(..), fromFoldable, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..), catMaybes, cons, cons')
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.NonEmpty ((:|))
import Data.Number (infinity, nan)
import Data.Number as Data.Number
import Data.String (toUpper)
import Data.String.CodePoints as SCP
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String.CodeUnits as SCU
import Data.String.Regex.Flags (RegexFlags, ignoreCase, noFlags)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (get2, (/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Unsafe (unsafePerformEffect)
import Node.Process (lookupEnv)
import Parsing (ParseError(..), ParseState(..), Parser, ParserT, Position(..), consume, fail, getParserT, initialPos, parseErrorPosition, position, region, runParser)
import Parsing.Combinators (advance, between, chainl, chainl1, chainr, chainr1, choice, empty, endBy, endBy1, lookAhead, many, many1, many1Till, many1Till_, manyIndex, manyTill, manyTill_, notFollowedBy, optionMaybe, replicateA, sepBy, sepBy1, sepEndBy, sepEndBy1, skipMany, skipMany1, try, tryRethrow, (<?>), (<??>), (<~?>))
import Parsing.Combinators.Array as Combinators.Array
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (haskellDef, haskellStyle, javaStyle)
import Parsing.String (anyChar, anyCodePoint, anyTill, char, eof, match, parseErrorHuman, regex, rest, satisfy, string, takeN)
import Parsing.String.Basic (intDecimal, letter, noneOfCodePoints, number, oneOfCodePoints, skipSpaces, takeWhile, takeWhile1, whiteSpace)
import Parsing.String.Basic as String.Basic
import Parsing.String.Replace (breakCap, replace, replaceT, splitCap, splitCapT)
import Parsing.Token (TokenParser, makeTokenParser, token, when)
import Parsing.Token as Token
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert', assertEqual')
import Test.IndentationTests as IndentationTests
import Test.IndentationTestsFields as IndentationTestsFields
import Test.Lib
import Test.Parser as Parser

main :: Effect Unit
main = do

  log "\nTESTS Indentation\n"
  -- IndentationTests.testIndentationParser
  IndentationTestsFields.test
  -- IndentationTestsFields.testIndentationParser

  -- log "\nTESTS Parser\n"
  -- Parser.test
