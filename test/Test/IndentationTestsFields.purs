module Test.IndentationTestsFields where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>>=))
import Control.Monad.State.Trans (get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty as NonEmptyArray
import Data.CodePoint.Unicode (isNumber)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), filterM, fromFoldable, mapMaybe)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), joinWith, take)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Parsing (ParseError(..), Position(..), eqPositionFull, fail, failWithPosition, position, runParserT)
import Parsing.Combinators (many, sepBy)
import Parsing.Combinators.Array as Combinators.Array
import Parsing.Indent (IndentParser, checkIndent, indentParens, indented, runIndent, withPos)
import Parsing.Indent as Indent
import Parsing.String (char, eof, string)
import Parsing.String.Basic (alphaNum, intDecimal, skipSpaces)
import Test.Assert (assert', assertEqual, assertEqual')
import Test.Lib (TestM, assertPosition)
import Test.Lib.IndentationTests (parseTest)
import Unsafe.Coerce (unsafeCoerce)

type Program_Success =
  { _1_start_go_and_then_put_withPos :: Array Char -- from start to 'p'
  , _2_go_and_then_indented :: Array Char -- from 'p' to 'o'
  , _3_go_and_then_eof :: Array Char -- from 'o' to end
  }

type Program_Fail =
  { _1_start_go_and_then_put_withPos :: Array Char -- from start to 'p'
  , _2_go_and_then_indented_and_then_fail :: Array Char -- from 'p' to 'x'
  }

splitAtFirstElementGoesToFst :: forall a . Eq a => a -> Array a -> Maybe (Tuple (Array a) (Array a))
splitAtFirstElementGoesToFst x arr =
  case Array.findIndex (_ == x) arr of
    Nothing -> Nothing
    Just idx -> Just $ Tuple (Array.take (idx + 1) arr) (Array.drop (idx + 1) arr)

parseField :: String -> Maybe (Tuple (NonEmptyArray Program_Success) (NonEmptyArray Program_Fail))
parseField = toCharArray >>> \field -> do
  (Tuple withP other) <- splitAtFirstElementGoesToFst 'p' field
  success <- ...
  fails <- ...
  pure (unsafeCoerce unit)


testProgram :: TestM
testProgram = do
  let assertEqual { field, expected } = assertEqual' (show field) { actual: parseField field, expected }
  assertEqual {
    field: String.joinWith "\n" [ " p" , "xo" ],
    expected: Just $ Tuple
      (NonEmptyArray.cons'
        { _1_start_go_and_then_put_withPos: " p" , _2_go_and_then_indented: "\nxo" , _3_go_and_then_eof: "" }
        []
      )
      (NonEmptyArray.cons'
        { _1_start_go_and_then_put_withPos: " p" , _2_go_and_then_indented_and_then_fail: "" } -- calling `indented` where `withPos` was called - should fail
        [ { _1_start_go_and_then_put_withPos: " p" , _2_go_and_then_indented_and_then_fail: "\n" }
        , { _1_start_go_and_then_put_withPos: " p" , _2_go_and_then_indented_and_then_fail: "\nx" }
        ]
      )
  }

-- Replace an element at a specific index in the array
replaceAt :: Int -> Char -> Array Char -> Array Char
replaceAt idx newChar arr = fromMaybe arr (Array.updateAt idx newChar arr)

-- Find all indices of 'o' in the array
findElementIndices :: forall a . Eq a => a -> Array a -> Array Int
findElementIndices a = Array.mapWithIndex (\i c -> if c == a then Just i else Nothing) >>> Array.catMaybes

-- -- " o o " => [" o   ", "   o "]
-- oToManyO :: String -> Array String
-- oToManyO = \input ->
--   let charArray = toCharArray input
--       permutations = oToManyOArray charArray
--   in map fromCharArray permutations
--   where
--     oToManyOArray :: Array Char -> Array (Array Char)
--     oToManyOArray input =
--       let oIndices = findElementIndices 'o' input
--       in map (
--         \indexToPreserve -> mapWithIndex (
--           \index char ->
--             if char == 'o'
--             then
--               if index == indexToPreserve
--               then char
--               else ' '
--             else char
--         ) input
--       ) oIndices

-- -- Helper function to move to a specific cell based on row and column
-- moveToCell :: Int -> Int -> IndentParser String Unit
-- moveToCell row column = void $ string $ take (row * column) field5on5
--
-- -- Helper function to move to the 'X' position in the field
-- moveToCellX :: IndentParser String Unit
-- moveToCellX = moveToCell 3 3 -- Moves to the center of the grid where 'X' is
--
-- -- moveToCellX = should moveToCell that is x on field5on5__moveToCell
--
-- -- | simple helper function to avoid typ-problems with MonadState instance
-- get' :: forall s. IndentParser s Position
-- get' = do
--   g <- lift get
--   pure g
--
-- assertCurrentPosition_Parser :: Position -> IndentParser String Unit
-- assertCurrentPosition_Parser expectedPosition = do
--   actualPosition <- position
--   if eqPositionFull expectedPosition actualPosition
--     then pure unit
--     else fail ("Parser -> expected position: " <> show expectedPosition <> ", actual position: " <> show actualPosition)
--
-- assertCurrentPosition_Indentation :: Position -> IndentParser String Unit
-- assertCurrentPosition_Indentation expectedPosition = do
--   actualPosition <- get'
--   if eqPositionFull expectedPosition actualPosition
--     then pure unit
--     else fail ("Indentation -> expected position: " <> show expectedPosition <> ", actual position: " <> show actualPosition)
--
-- data StringFromXtoOError
--   = StringFromXtoOError_LengthsNotSame
--   | StringFromXtoOError_XInputIsInvalid -- should contain only 1 x (required), spaces, newlines, numbers
--   | StringFromXtoOError_OInputIsInvalid -- should contain only 1 o (required), spaces, newlines, numbers
--   | StringFromXtoOError_OisBeforeX
--
-- derive instance Generic StringFromXtoOError _
-- derive instance Eq StringFromXtoOError
-- instance Show StringFromXtoOError where
--   show x = genericShow x
--
-- stringFromStartTo :: Char -> String -> Maybe String
-- stringFromStartTo x xStr =
--   let
--     xArray = toCharArray xStr
--   in
--     if isValidInput xArray
--       then
--         case findElementIndices x xArray of
--           [xIndex] -> Just $ String.replace (Pattern $ CodeUnits.singleton x) (Replacement " ") $ fromCharArray $ Array.slice 0 (xIndex + 1) xArray
--           _ -> Nothing
--       else
--         Nothing
--   where
--     isValidInput :: Array Char -> Boolean
--     isValidInput arr =
--       Array.all (\c -> c == x || isNumber (codePointFromChar c) || c == ' ' || c == '\n') arr
--
-- stringFromTo :: Char -> Char -> String -> String -> Either StringFromXtoOError String
-- stringFromTo x o xStr oStr =
--   let
--     xArray = toCharArray xStr
--     oArray = toCharArray oStr
--     lenX = Array.length xArray
--     lenO = Array.length oArray
--   in
--     if lenX /= lenO then
--       Left StringFromXtoOError_LengthsNotSame
--     else
--       case findElementIndices x xArray, findElementIndices o oArray of
--         [xIndex], [oIndex] ->
--           if xIndex > oIndex then
--             Left StringFromXtoOError_OisBeforeX
--           else if not (isValidInput xArray) then
--             Left StringFromXtoOError_XInputIsInvalid
--           else if not (isValidInput oArray) then
--             Left StringFromXtoOError_OInputIsInvalid
--           else
--             Right $ String.replace (Pattern $ CodeUnits.singleton x) (Replacement " ") $ fromCharArray $ Array.slice (xIndex + 1) (oIndex + 1) xArray
--         [_xIndex], _ -> Left StringFromXtoOError_OInputIsInvalid
--         _, _ -> Left StringFromXtoOError_XInputIsInvalid
--   where
--     isValidInput :: Array Char -> Boolean
--     isValidInput arr =
--       Array.all (\c -> c == x || c == o || isNumber (codePointFromChar c) || c == ' ' || c == '\n') arr

test :: TestM
test = do
  testProgram
  -- testoToManyO
  -- testStringFromXtoO
  -- testStringFromStartToX
  -- testIndentationParser

-- testoToManyO :: TestM
-- testoToManyO = do
--   let assertEqual { input, expected } = assertEqual' (show input) { actual: oToManyO input, expected }
--   assertEqual { input: " o o ", expected: [" o   ", "   o "] }
--
-- testStringFromXtoO :: TestM
-- testStringFromXtoO = do
--   let assertEqual { xStr, expected } = assertEqual' (show xStr <> " " <> show expected) { actual: stringFromStartTo 'x' xStr, expected }
--   assertEqual { xStr: " xx  ", expected: Nothing }
--   assertEqual { xStr: "x2345", expected: Just " " }
--   assertEqual { xStr: "1x345", expected: Just "1 " }
--
-- testStringFromStartToX :: TestM
-- testStringFromStartToX = do
--   let assertEqualXO { xStr, oStr, expected } = assertEqual' (show xStr <> " " <> show oStr <> " " <> show expected) { actual: stringFromTo 'x' 'o' xStr oStr, expected }
--   assertEqualXO { xStr: " x   ", oStr: "o     ", expected: Left StringFromXtoOError_LengthsNotSame }
--   assertEqualXO { xStr: "1x345", oStr: "o    ", expected: Left StringFromXtoOError_OisBeforeX }
--   assertEqualXO { xStr: "1x345", oStr: " o   ", expected: Right "" }
--   assertEqualXO { xStr: "1x345", oStr: "  o  ", expected: Right "3" }
--   assertEqualXO { xStr: "1x345", oStr: "   o ", expected: Right "34" }
--   assertEqualXO { xStr: "1x345", oStr: "    o", expected: Right "345" }
--   assertEqualXO { xStr: "1x\n345", oStr: "  \n o ", expected: Right "\n34" }
--
-- testIndentationParser :: TestM
-- testIndentationParser = do
--   (movementFromStartToX :: String) <- maybe (throw "stringFromStartToX field5on5__moveToCell is Nothing") pure $ stringFromStartTo 'x' field5on5__moveToCell
--
--   log $ show movementFromStartToX
--
--   let os = oToManyO field5on5__testsShouldShowThatAllCellsExceptOfTheseWillThrowError
--   log $ show field5on5__moveToCell
--   log $ show field5on5__testsShouldShowThatAllCellsExceptOfTheseWillThrowError
--   log $ show os
--
--   for_ os \o_field -> do
--     log $ show o_field
--
--     (movementFromXtoO :: String) <-
--       case stringFromTo 'x' 'o' field5on5__moveToCell o_field of
--         Right movement -> pure movement
--         Left error ->
--           case error of
--             StringFromXtoOError_LengthsNotSame -> throw "StringFromXtoOError_LengthsNotSame"
--             StringFromXtoOError_XInputIsInvalid -> throw "StringFromXtoOError_XInputIsInvalid" -- should contain only 1 x (required), spaces, newlines, numbers
--             StringFromXtoOError_OInputIsInvalid -> throw "StringFromXtoOError_OInputIsInvalid" -- should contain only 1 o (required), spaces, newlines, numbers
--             StringFromXtoOError_OisBeforeX -> throw "StringFromXtoOError_OisBeforeX"
--
--     log $ show movementFromXtoO
--
--     let
--       (parserResult :: Either ParseError Unit) = runIndent $ runParserT field5on5 do
--         assertCurrentPosition_Parser (Position { index: 0, line: 1, column: 1 })
--         assertCurrentPosition_Indentation (Position { index: 0, line: 1, column: 1 })
--
--         void $ string $ movementFromStartToX
--
--         assertCurrentPosition_Parser (Position { column: 4, index: 15, line: 3 })
--         assertCurrentPosition_Indentation (Position { index: 0, line: 1, column: 1 })
--
--         withPos do
--           -- assertCurrentPosition_Parser (Position { column: 4, index: 15, line: 3 })
--           -- assertCurrentPosition_Indentation (Position { column: 4, index: 15, line: 3 })
--           void $ string movementFromXtoO
--           -- assertCurrentPosition_Parser (Position { column: 5, index: 22, line: 4 })
--           -- assertCurrentPosition_Indentation (Position { column: 4, index: 15, line: 3 })
--           indented
--           -- assertCurrentPosition_Parser (Position { column: 5, index: 22, line: 4 })
--           -- assertCurrentPosition_Indentation (Position { column: 4, index: 0, line: 4 })
--
--     -- Check the result
--     case parserResult of
--       Right _ -> assert' "Parser succeeded as expected" true
--       Left err -> assert' ("Parser error: " <> show err) false
--
--   -- for each cell from x to the end \testedPosition -> do
--   --   (parserResult :: Either ParseError Unit) <- runParser field5on5 do
--   --     get' >>= \actualPosition -> lift $ lift $ assertPosition (Position { index: 0, line: 1, column: 1 }) actualPosition
--   --     moveToCellWithX field5on5__moveToCell
--   --     get' >>= \actualPosition -> lift $ lift $ assertPosition (Position { index: 0, line: 3, column: 3 }) actualPosition
--   --     moveToCell testedPosition.row testedPosition.column
--   --     withPos do
--   --       indented do -- can throw error
--   --         string "to the end"
--   --         eof
--   --
--   --   case parserResult, shouldPass of
--   --     Right unit, true -> assert' ("expected output: " <> show expected <> ", actual output: " <> show actual) (expected == actual)
--   --     Left err, true -> assert' (joinWith "\n" $ ["error: " <> show err] <> parseErrorHuman__onlyString input 60 err) false
--   --     Right unit, false -> assert' ("expected output: " <> show expected <> ", actual output: " <> show actual) (expected == actual)
--   --     Left err, false -> assert' (joinWith "\n" $ ["error: " <> show err] <> parseErrorHuman__onlyString input 60 err) false
