module Inference.Client where

import qualified Data.ByteString as BS
import Inference.Codec
  ( deserializeResponse,
    errorResponseBodyLength,
    serializeRequest,
    successResponseBodyLength,
  )
import Inference.Protocol
  ( Request,
    Response,
  )
import System.IO

data Connection = Connection
  { cxnWrite :: BS.ByteString -> IO (),
    cxnRead :: Int -> IO BS.ByteString
  }

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
