module Mopedi.WeeChatParser where

import Prelude

import Control.Alternative (empty, guard)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List (uncons)
import Data.Array (length, fromFoldable)
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Cast (toUint8Array)
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.String.CodePoints (codePointFromChar)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (catchException, message)
import Parsing (runParserT, runParser, fail, ParserT, ParseError, liftExceptT, liftMaybe)
import Parsing.Combinators (replicateA, sepBy, optional)
import Parsing.String as String
import Parsing.String.Basic (takeWhile)
import Parsing.DataView (anyInt32be, anyInt8, satisfyInt8, takeN)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel

data WeeChatMessage = Buffers (Array Buffer) | History (Array HistoryRow)

derive instance genericWeeChatMessage :: Generic WeeChatMessage _

instance showWeeChatMessage :: Show WeeChatMessage where
  show = genericShow

type Buffer =
  { ppath :: String
  , number :: Int
  , fullName :: String
  , shortName :: Maybe String
  }

type HistoryRow =
  { message :: Array ColoredString
  , buffer :: String
  , date :: String
  , prefix :: Maybe String
  }

type ColoredString = { color :: Int, content :: String }

parseWeeChatMsg :: ArrayBuffer -> Effect (Either ParseError WeeChatMessage)
parseWeeChatMsg msg = runParserT (DataView.whole msg) parser

type Parser a = ParserT DataView Effect a

parser :: Parser WeeChatMessage
parser = do
  _length <- anyInt32be
  _compression <- satisfyInt8 (_ == 0x00)
  id <- parseString
  case id of
    Just "buffers" -> Buffers <$> parseBuffers
    Just "history" -> History <$> parseHistory
    _ -> fail $ "No parser implemented for message id: " <> (fromMaybe "<empty id>" id)

parseBuffers :: Parser (Array Buffer)
parseBuffers = do
  dataType <- parseStringN 3
  guard $ dataType == "hda"
  _hpath <- parseNonEmptyString
  keys <- parseNonEmptyString
  -- keys are set in the implementation of WeeChatAppM
  guard $ keys == "number:int,full_name:str,short_name:str"
  count <- anyInt32be
  buffers :: Array Buffer <- replicateA count do
    ppath <- parseShortString
    number <- anyInt32be
    fullName <- parseNonEmptyString
    shortName <- parseString
    pure { ppath, number, fullName, shortName }
  pure buffers

parseHistory :: Parser (Array HistoryRow)
parseHistory = do
  dataType <- parseStringN 3
  guard $ dataType == "hda"
  hpath <- parseNonEmptyString
  let pathLength = length $ split (Pattern "/") hpath
  keys <- parseNonEmptyString
  guard $ keys == "message:str,buffer:ptr,date:tim,prefix:str"
  count <- anyInt32be
  history :: Array HistoryRow <- replicateA count do
    _ppath :: Array String <- replicateA pathLength parseShortString
    message <- parseColoredString
    buffer <- parseShortString
    date <- parseShortString
    prefix <- parseString
    pure { message, buffer, date, prefix }
  pure history

parseNonEmptyString :: Parser String
parseNonEmptyString =
  parseString >>= maybe empty pure

parseString :: Parser (Maybe String)
parseString = do
  length <- anyInt32be
  -- length 0xFFFFFFFF means Null string in WeeChat.
  case length of
    -1 -> pure Nothing
    0 -> pure $ Just ""
    _ -> Just <$> parseStringN length

parseShortString :: Parser String
parseShortString = anyInt8 >>= parseStringN

parseStringN :: Int -> Parser String
parseStringN length = do
  stringview <- takeN length
  stringarray <- lift $ liftEffect $ toUint8Array stringview
  -- Why is creating a TextDecoder an effect?
  textDecoder <- lift $ liftEffect $ TextDecoder.new UtfLabel.utf8
  liftExceptT $ ExceptT $ catchException (pure <<< Left <<< message) do
    Right <$> TextDecoder.decode stringarray textDecoder

parseColoredString :: Parser (Array ColoredString)
parseColoredString = do
  str <- parseNonEmptyString
  liftEither $ runParser str do
    let
      sep = ''
      sepCodePoint = codePointFromChar sep
      parseTillSep = takeWhile (_ /= sepCodePoint)
    arr <- parseTillSep `sepBy` String.char sep
    { head, tail } <- liftMaybe (const "impossible!") $ uncons arr
    let
      headColors =
        case head of
          "" -> []
          s -> [ { color: 0, content: s } ]
    tailColors <- liftEither $ traverse (flip runParser parseColor) tail
    pure $ headColors <> fromFoldable tailColors
  where
  parseColor = do
    -- What does the star do?
    _ <- optional $ String.char '*'
    -- What does the F do?
    _ <- optional $ do
      _ <- String.char 'F'
      optional $ String.char '1'
    colorCode <- String.takeN 2
    color <- liftMaybe (const $ "Color was not an integer: " <> colorCode) $ fromString colorCode
    content <- String.rest
    pure { color, content }

