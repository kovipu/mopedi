module Mopedi.WeeChatParser where

import Prelude

import Control.Alternative (empty, guard)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (length, fromFoldable)
import Data.ArrayBuffer.Cast (toUint8Array)
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List (uncons)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.CodePoints (codePointFromChar)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (catchException, message)
import Parsing (ParseError, ParserT, fail, liftExceptT, liftMaybe, runParser, runParserT)
import Parsing.Combinators (replicateA, sepBy, optional, (<|>))
import Parsing.DataView (anyInt32be, anyInt8, satisfyInt8, takeN)
import Parsing.String as String
import Parsing.String.Basic (takeWhile)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel

data WeeChatMessage = Buffers (Array Buffer) | History (Array HistoryRow) | NewLine HistoryRow

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
  , prefix :: Array ColoredString
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
    Just "_buffer_line_added" -> NewLine <$> parseNewLine
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
    prefix <- parseColoredString
    pure { message, buffer, date, prefix }
  pure history

parseNewLine :: Parser HistoryRow
parseNewLine = do
  dataType <- parseStringN 3
  guard $ dataType == "hda"
  path <- parseNonEmptyString
  guard $ path == "line_data"
  keys <- parseNonEmptyString
  guard $ keys == "buffer:ptr,date:tim,date_printed:tim,displayed:chr,notify_level:chr,highlight:chr,tags_array:arr,prefix:str,message:str"
  count <- anyInt32be
  guard $ count == 1 -- can this message have >1 lines?
  _ppath <- parseShortString
  buffer <- parseShortString
  date <- parseShortString
  _date_printed <- parseShortString
  _displayed <- anyInt8
  _notify_level <- anyInt8 -- TODO: show notifications.
  _highlight <- anyInt8 -- TODO: show highlights.
  tagsType <- parseStringN 3
  guard $ tagsType == "str"
  tagsCount <- anyInt32be
  _tags_array :: Array String <- replicateA tagsCount parseNonEmptyString
  prefix <- parseColoredString
  message <- parseColoredString
  pure { message, buffer, date, prefix }

parseChar :: Parser Char
parseChar = do
  int <- anyInt8
  liftMaybe (const "invalid charcode") $ fromCharCode int

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
  str <- parseString
  case str of
    Nothing -> pure []
    Just s ->
      liftEither $ runParser s do
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
              h -> [ { color: 0, content: h } ]
        tailColors <- liftEither $ traverse (flip runParser parseColor) tail
        pure $ headColors <> fromFoldable tailColors
      where
      parseColor = do
        -- What do these characters do?
        _ <- optional $ String.char '*'
        _ <- optional $ String.char 'F'
        -- TODO: find a way to find the colorCode length.
        color <- parseLongColor <|> parseNumberN 2
        content <- String.rest
        pure { color, content }

      parseLongColor = do
        _ <- String.char '@'
        parseNumberN 5

      parseNumberN n = do
        colorCode <- String.takeN n
        liftMaybe (const $ "Color was not an integer: " <> colorCode) $ fromString colorCode
