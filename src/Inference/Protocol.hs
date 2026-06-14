module Inference.Protocol where

import Data.Text (Text)
import Data.Word (
  Word64,
  Word8,
 )
import Inference.Error (InferenceErrorCode, wordToInferenceErrorCode)

data Opcode
  = Perplexity
  | CrossEntropy
  | UnknownOpcode

opcodeToWord :: Opcode -> Word8
opcodeToWord Perplexity = 1
opcodeToWord CrossEntropy = 2
opcodeToWord UnknownOpcode = 3

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
okResponse responseId value =
  Response
    { responseId = responseId
    , value = Right value
    }

errorResponse :: Word64 -> Word8 -> Response
errorResponse responseId errorCode =
  Response
    { responseId = responseId
    , value = Left $ wordToInferenceErrorCode errorCode
    }
