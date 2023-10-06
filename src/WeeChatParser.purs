module Mopedi.WeeChatParser where

import Prelude

import Control.Alternative (empty, guard)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Cast (toUint8Array)
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (catchException, message)
import Parsing (runParserT, ParserT, ParseError, liftExceptT)
import Parsing.Combinators (replicateA)
import Parsing.DataView (anyInt32be, anyInt8, satisfyInt8, takeN)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel

data WeeChatMessage = Buffers (Array Buffer)

derive instance genericWeeChatMessage :: Generic WeeChatMessage _

instance showWeeChatMessage :: Show WeeChatMessage where
  show = genericShow

type Buffer =
  { ppath :: String
  , number :: Int
  , fullName :: String
  , shortName :: Maybe String
  }

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
    _ -> empty

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
    ppath <- anyInt8 >>= parseStringN
    number <- anyInt32be
    fullName <- parseNonEmptyString
    shortName <- parseString
    pure { ppath, number, fullName, shortName }
  pure buffers

parseNonEmptyString :: Parser String
parseNonEmptyString =
  parseString >>= maybe empty pure

parseString :: Parser (Maybe String)
parseString = do
  length <- anyInt32be
  -- length 0xFFFF means Null string in WeeChat.
  case length of
    -1 -> pure Nothing
    0 -> pure $ Just ""
    _ -> Just <$> parseStringN length

parseStringN :: Int -> Parser String
parseStringN length = do
  stringview <- takeN length
  stringarray <- lift $ liftEffect $ toUint8Array stringview
  -- Why is creating a TextDecoder an effect?
  textDecoder <- lift $ liftEffect $ TextDecoder.new UtfLabel.utf8
  liftExceptT $ ExceptT $ catchException (pure <<< Left <<< message) do
    Right <$> TextDecoder.decode stringarray textDecoder

