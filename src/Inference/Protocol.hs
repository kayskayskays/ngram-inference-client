module Inference.Protocol where 

data Opcode
  = Perplexity
  | CrossEntropy
  | UnknownOpcode

data InferenceErrorCode
  = InvalidOpcode
  | GarbageArgs
  | InvalidUtf8
  | InferenceError

data Request = Request 
  { opcode :: Opcode
  , text :: String 
  }

newtype Response = Response (Either InferenceErrorCode Double)