module Inference.Error where

import Data.Binary (Word8)

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

wordToInferenceErrorCode :: Word8 -> InferenceErrorCode
wordToInferenceErrorCode 1 = InvalidOpcode
wordToInferenceErrorCode 2 = GarbageArgs
wordToInferenceErrorCode 3 = InvalidUtf8
wordToInferenceErrorCode 4 = InferenceError
wordToInferenceErrorCode 5 = ConnectionError
wordToInferenceErrorCode _ = UnknownError
