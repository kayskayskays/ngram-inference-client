{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Inference.Client where

import Control.Monad.State
  ( MonadIO (liftIO),
    MonadState (get, put),
    StateT,
    evalStateT,
  )
import Control.Monad.Trans (lift)
import Data.Binary (Word64)
import qualified Data.ByteString as BS
import Data.Text (Text, pack)
import Inference.Codec
  ( deserializeResponse,
    errorResponseBodyLength,
    serializeRequest,
    successResponseBodyLength,
  )
import Inference.Protocol
  ( InferenceErrorCode,
    Opcode (Perplexity),
    Request (..),
    Response (Response, value),
  )
import System.IO (Handle)

data Connection = Connection
  { cxnWrite :: BS.ByteString -> IO (),
    cxnRead :: Int -> IO BS.ByteString
  }

type RequestId = Word64

type Client m a = StateT RequestId m a

type PartialRequest = RequestId -> Request

class (Monad m) => QuoteRepository m where
  nextQuote :: m Text

connectionFromHandle :: Handle -> Connection
connectionFromHandle handle =
  Connection
    { cxnWrite = BS.hPut handle,
      cxnRead = BS.hGet handle
    }

sendRequest :: Connection -> Request -> IO ()
sendRequest = (. serializeRequest) . cxnWrite

receiveResponse :: Connection -> IO (Either String Response)
receiveResponse cxn = do
  status <- cxnRead cxn 1
  case BS.head status of
    0 -> deserializeResponse . (status <>) <$> cxnRead cxn successResponseBodyLength
    1 -> deserializeResponse . (status <>) <$> cxnRead cxn errorResponseBodyLength
    _ -> pure $ Left "unknown status"

search :: (QuoteRepository m, MonadIO m) => Connection -> m Response
search cxn =
  evalStateT search0 0
  where
    search0 :: (QuoteRepository m, MonadIO m) => Client m Response
    search0 = do
      quote <- lift nextQuote
      result <- perplexity cxn quote

      case result of
        Left err -> search0
        Right response ->
          if testResponse response
            then pure response
            else search0

threshold :: (Double, Double)
threshold = (5, 90)

testResponse :: Response -> Bool
testResponse (Response _ (Right value)) = fst threshold > value && value < snd threshold
testResponse _ = False

perplexity :: (MonadIO m, MonadState RequestId m) => Connection -> Text -> m (Either String Response)
perplexity cxn txt = do
  request <- prepareRequest $ \requestId ->
    Request
      { requestId = requestId,
        opcode = Perplexity,
        text = txt
      }
  clientSend cxn request

clientSend :: (MonadIO m) => Connection -> Request -> m (Either String Response)
clientSend cxn request = do
  liftIO $ sendRequest cxn request
  liftIO $ receiveResponse cxn

prepareRequest :: (MonadIO m, MonadState RequestId m) => PartialRequest -> m Request
prepareRequest partial = do
  requestId <- allocateId
  pure $ partial requestId

allocateId :: (MonadState RequestId m) => m RequestId
allocateId = do
  currentId <- get
  put (currentId + 1)
  pure currentId
