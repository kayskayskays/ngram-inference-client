module Inference.Client where

import Data.ByteString
import System.IO

import Inference.Protocol (
    Request
  , Response
  )

data Channel = Channel
  { sendRequest     :: Request -> IO ()
  , receiveResponse :: IO Response
  }
