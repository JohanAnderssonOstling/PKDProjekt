module Server where
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Network.Socket
import Data.Map (Map)
import qualified Data.Map       as Map
type Username = String
type UserPassword = String
type Msg = (Username, String)

data ClientState = ClientState {
    quit :: Bool,
    muted :: Map Username Bool
}

main :: IO ()
main = do
    newTVarIO Map.empty
    putStrLn "Creating socket"
    sock <- socket AF_INET Stream 0 -- Create a socket
    setSocketOption sock ReuseAddr 1 -- Set socket reuse on
    bind sock (SockAddrInet 18000 0) -- Bind socket to port 10000
    listen sock 2 -- Listen for connections
    chan <- newChan
    serverLoop sock chan
    close sock

serverLoop :: Socket -> Chan String -> IO ()
serverLoop socket chan = do
    connection <- accept socket
    forkIO (handleClient connection chan) -- Create a new thread for the accepted client
    serverLoop socket chan

handleClient :: (Socket, socketAddr) -> Chan String -> IO ()
handleClient (socket, _) chan = do
    
    handle <- socketToHandle socket ReadWriteMode -- Create a sockethandle for client communication
    username <- hGetLine handle
    password <- hGetLine handle
    forkIO (outLoop handle chan)
    inLoop handle chan
    hClose handle
    close socket

inLoop :: Handle -> Chan String -> IO ()
inLoop handle chan = do
    inLine <- hGetLine handle
    putStrLn inLine
    writeChan chan inLine
    inLoop handle chan

outLoop :: Handle -> Chan String -> IO ()
outLoop handle chan = do
    
    broadChan <- dupChan chan
    outLine <- readChan broadChan
    hPutStrLn handle ("Output:" ++ outLine)
    outLoop handle chan