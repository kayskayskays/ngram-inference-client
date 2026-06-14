{-# LANGUAGE FlexibleContexts #-}

module Inference.Client where

import Control.Monad.State (
  MonadIO (liftIO),
  MonadState (get, put),
  StateT (StateT, runStateT),
  evalStateT,
  modify,
 )
import Control.Monad.Trans (lift)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary (Word64)
import qualified Data.ByteString as BS
import Data.Text (Text, pack)
import GHC.TypeError (ErrorMessage (Text))
import Inference.Codec (
  deserializeResponse,
  errorResponseBodyLength,
  serializeRequest,
  successResponseBodyLength,
 )
import Inference.Error (CodecError (DeserializationError), ProtocolError (CodecError, ExceededAttemptLimit, OutOfOrderError))
import Inference.Protocol (
  Opcode (Perplexity),
  Request (..),
  Response (Response, responseId, value),
 )
import System.IO (Handle)

data Connection = Connection
  { cxnWrite :: BS.ByteString -> IO ()
  , cxnRead :: Int -> IO BS.ByteString
  }

newtype QuoteRepository m = QuoteRepository
  { nextQuote :: m Text
  }

type RequestId = Word64

connectionFromHandle :: Handle -> Connection
connectionFromHandle handle =
  Connection
    { cxnWrite = BS.hPut handle
    , cxnRead = BS.hGet handle
    }

search :: (MonadIO m) => QuoteRepository m -> Connection -> m (Either ProtocolError Text)
search repo cxn =
  evalStateT (search0 repo) (0, 0)
 where
  search0 :: (MonadIO m) => QuoteRepository m -> StateT (Int, RequestId) m (Either ProtocolError Text)
  search0 repo = do
    modify $ \(count, reqId) -> (count + 1, reqId)
    currentCount <- fst <$> get

    quote <- lift $ nextQuote repo
    result <- liftRequestIdState $ perplexity cxn quote

    case result of
      Right response
        | testResponse response -> pure $ Right quote
        | currentCount > maxAttempts -> pure $ Left ExceededAttemptLimit
        | otherwise -> search0 repo
      Left err -> pure $ Left err

{- Lifts a `StateT RequestId m` into a `StateT (Int, RequestId) m`. -}
liftRequestIdState :: (MonadIO m) => StateT RequestId m a -> StateT (Int, RequestId) m a
liftRequestIdState requestIdState = StateT $ \(count, reqId) -> do
  (result, reqId') <- runStateT requestIdState reqId
  pure (result, (count, reqId'))

maxAttempts :: Int
maxAttempts = 100

threshold :: (Double, Double)
threshold = (5, 90)

testResponse :: Response -> Bool
testResponse (Response _ (Right value)) = fst threshold > value && value < snd threshold
testResponse _ = False

perplexity ::
  (MonadIO m, MonadState RequestId m) => Connection -> Text -> m (Either ProtocolError Response)
perplexity cxn txt = do
  request <- prepareRequest $ \requestId ->
    Request
      { requestId = requestId
      , opcode = Perplexity
      , text = txt
      }
  clientSend cxn request

sendRequest :: Connection -> Request -> IO ()
sendRequest = (. serializeRequest) . cxnWrite

receiveResponse :: Connection -> IO (Either ProtocolError Response)
receiveResponse cxn = do
  status <- cxnRead cxn 1
  bimap CodecError id
    <$> case BS.head status of -- Mapping `ProtocolError` over the `ConnectionError`
      0 -> deserializeResponse . (status <>) <$> cxnRead cxn successResponseBodyLength
      1 -> deserializeResponse . (status <>) <$> cxnRead cxn errorResponseBodyLength
      _ -> pure $ Left $ DeserializationError "unknown status"

clientSend :: (MonadIO m) => Connection -> Request -> m (Either ProtocolError Response)
clientSend cxn request = do
  liftIO $ sendRequest cxn request
  response <- liftIO $ receiveResponse cxn
  let validatedResponse = validateOrdering (requestId request) response
  pure validatedResponse

validateOrdering :: RequestId -> Either ProtocolError Response -> Either ProtocolError Response
validateOrdering _ result@(Left _) = result
validateOrdering expected result@(Right resp)
  | (responseId resp /= expected) = Left OutOfOrderError
  | otherwise = result

type PartialRequest = RequestId -> Request

prepareRequest :: (MonadIO m, MonadState RequestId m) => PartialRequest -> m Request
prepareRequest partial = do
  requestId <- allocateId
  pure $ partial requestId

allocateId :: (MonadState RequestId m) => m RequestId
allocateId = do
  currentId <- get
  put (currentId + 1)
  pure currentId
