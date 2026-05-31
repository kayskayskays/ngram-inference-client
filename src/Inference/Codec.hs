module Inference.Codec where

import Inference.Protocol (Request (..), opcodeToWord, Response (Response), okResponse, errorResponse, InferenceErrorCode, wordToInferenceErrorCode)
import qualified Data.ByteString as BS
import Data.Word (Word32, Word64, Word8)
import Data.Text.Encoding (encodeUtf8)
import Data.Binary.Put (runPut, putWord32le, putWord64le, putWord8, putByteString)
import Data.Binary.Get (runGet, getWord8, Get, runGetOrFail, getWord64be, getWord64le, getDoublele)

requestPrefixLength :: Word32
requestPrefixLength = 13

requestLength :: BS.ByteString -> Word32
requestLength = (+requestPrefixLength) . fromIntegral . BS.length

serializeRequest :: Request -> BS.ByteString
serializeRequest request = BS.toStrict $ runPut $ do
  let encodedText = encodeRequestText request
  let totalLength = requestLength encodedText
  putWord32le totalLength
  putWord64le $ requestId request
  putWord8 $ opcodeToWord $ opcode request
  putByteString $ encodedText

encodeRequestText :: Request -> BS.ByteString
encodeRequestText = encodeUtf8 . text
    
deserializeResponse :: BS.ByteString -> Either String Response
deserializeResponse byteString = case runGetOrFail deserialize (BS.fromStrict byteString) of
  Left (_, _, err) -> Left err
  Right (_, _, val) -> Right val
  where 
    deserialize = do 
      status <- getWord8
      id <- getWord64le
      let partialResponse = Response id
      case status of
        1 -> deserializeErrorResponse partialResponse
        0 -> deserializeSuccessResponse partialResponse
        _ -> fail "unknown error code"

type PartialResponse = Either InferenceErrorCode Double -> Response

deserializeSuccessResponse :: PartialResponse -> Get Response
deserializeSuccessResponse = (<$> getDoublele) . (. Right)

deserializeErrorResponse :: PartialResponse -> Get Response
deserializeErrorResponse = (<$> getWord8) . (. Left . wordToInferenceErrorCode)