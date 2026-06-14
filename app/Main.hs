module Main (main) where

import Data.Text (Text, pack)
import GHC.IO (bracket)
import GHC.IO.Handle (Handle, hClose)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Inference.Client (QuoteRepository (QuoteRepository, nextQuote), connectionFromHandle, search)
import Inference.Error (ProtocolError, protocolErrorString)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  SocketType (Stream),
  connect,
  defaultProtocol,
  socket,
  socketToHandle,
 )

main :: IO ()
main = do
  searchResult <-
    bracket
      openConnection
      hClose
      $ \handle -> do
        let cxn = connectionFromHandle handle
        let repo = constantRepo $ pack "hi"
        search repo cxn
  print $ searchResultToString searchResult

searchResultToString :: Either ProtocolError Text -> Text
searchResultToString (Left err) = protocolErrorString err
searchResultToString (Right quote) = quote

openConnection :: IO Handle
openConnection = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix "/tmp/ngram.sock")
  socketToHandle sock ReadWriteMode

constantRepo :: (Applicative m) => Text -> QuoteRepository m
constantRepo quote =
  QuoteRepository
    { nextQuote = pure quote
    }
