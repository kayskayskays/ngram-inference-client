module Inference.Error where

import Data.Binary (Word8)
import Data.Text (Text, pack)

data InferenceErrorCode
  = InvalidOpcode
  | GarbageArgs
  | InvalidUtf8
  | InferenceError
  | ConnectionError
  | UnknownError

data CodecError
  = DeserializationError String

data ProtocolError
  = CodecError CodecError
  | OutOfOrderError
  | ExceededAttemptLimit

protocolErrorString :: ProtocolError -> Text
protocolErrorString err = pack $ case err of 
    CodecError codecErr -> case codecErr of
        DeserializationError s -> s
    OutOfOrderError -> "received an out-of-order response"
    ExceededAttemptLimit -> "exceeded max quote generation attempts"


wordToInferenceErrorCode :: Word8 -> InferenceErrorCode
wordToInferenceErrorCode 1 = InvalidOpcode
wordToInferenceErrorCode 2 = GarbageArgs
wordToInferenceErrorCode 3 = InvalidUtf8
wordToInferenceErrorCode 4 = InferenceError
wordToInferenceErrorCode 5 = ConnectionError
wordToInferenceErrorCode _ = UnknownError
