{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.Client where

import qualified Data.ByteString as BS
import Inference.Codec
  ( deserializeResponse,
    errorResponseBodyLength,
    serializeRequest,
    successResponseBodyLength,
  )
import Inference.Protocol
  ( Request (..),
    Response (value, Response), Opcode (Perplexity), InferenceErrorCode,
  )
import System.IO
import Data.Text (Text, pack)
import Data.Binary (Word64)
import Control.Monad.State (StateT (runStateT), MonadState (..), MonadIO (liftIO), evalStateT)

data Connection = Connection
  { cxnWrite :: BS.ByteString -> IO (),
    cxnRead :: Int -> IO BS.ByteString
  }

type RequestId = Word64
type Client a = StateT RequestId IO a
type PartialRequest = RequestId -> Request

data QuoteRepository = Nothing

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

nextQuote :: QuoteRepository -> Text
nextQuote repo = pack ""

threshold :: (Double, Double)
threshold = (5, 90)

search :: Connection -> QuoteRepository -> IO Response
search cxn repo = 
  evalStateT search0 0
  where
    search0 :: Client Response
    search0 = do
      let quote = nextQuote repo

      request <- prepareRequest $ \requestId -> 
        Request 
          { requestId=requestId
          , opcode=Perplexity
          , text=quote 
          }

      result <- clientSend cxn request

      case result of
        Left err -> search0

        Right response -> 
          if withinThreshold threshold response
            then pure response
            else search0

withinThreshold :: (Double, Double) -> Response -> Bool
withinThreshold threshold (Response _ (Right value)) = fst threshold > value && value < snd threshold
withinThreshold _ _ = False

clientSend :: Connection -> Request -> Client (Either String Response)
clientSend cxn request = do 
  liftIO $ sendRequest cxn request
  liftIO $ receiveResponse cxn

prepareRequest :: PartialRequest -> Client Request
prepareRequest partial = do
  requestId <- allocateId
  pure $ partial requestId

allocateId :: Client RequestId
allocateId = do
  currentId <- get
  put (currentId + 1)
  pure currentId
