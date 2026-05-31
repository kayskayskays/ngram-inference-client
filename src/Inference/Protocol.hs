module Inference.Protocol where

import Data.Word
import Data.Text (Text)

data Opcode
  = Perplexity
  | CrossEntropy
  | UnknownOpcode

opcodeToWord :: Opcode -> Word8
opcodeToWord Perplexity    = 1
opcodeToWord CrossEntropy  = 2
opcodeToWord UnknownOpcode = 3

data InferenceErrorCode
  = InvalidOpcode
  | GarbageArgs
  | InvalidUtf8
  | InferenceError
  | ConnectionError
  | UnknownError

wordToInferenceErrorCode :: Word8 -> InferenceErrorCode 
wordToInferenceErrorCode 1 = InvalidOpcode
wordToInferenceErrorCode 2 = GarbageArgs
wordToInferenceErrorCode 3 = InvalidUtf8
wordToInferenceErrorCode 4 = InferenceError
wordToInferenceErrorCode 5 = ConnectionError
wordToInferenceErrorCode _ = UnknownError


data Request = Request 
  { requestId :: Word64
  , opcode :: Opcode
  , text :: Text 
  }

data Response = Response
  { responseId :: Word64
  , value :: Either InferenceErrorCode Double
  }

okResponse :: Word64 -> Double -> Response
okResponse responseId value = Response { responseId = responseId, value = Right value }

errorResponse :: Word64 -> Word8 -> Response
errorResponse responseId errorCode = Response { responseId = responseId, value = Left $ wordToInferenceErrorCode errorCode }