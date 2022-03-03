module Server where
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Network.Socket
import Data.Map (Map)
import qualified Data.Map       as Map
import System.Environment 
import ParseInput

data Msg = Msg {
    sender :: Username,
    reciever :: Username,
    content :: String
}



main :: IO ()
main = do
    putStrLn "Enter port"
    port <- getLine
    startServer port

startServer :: String -> IO ()
startServer port = do
    putStrLn ("Starting server on port: " ++ port)
    sock <- socket AF_INET Stream 0 -- Create a socket
    --setSocketOption sock ReuseAddr 0 -- Set socket reuse off
    bind sock (SockAddrInet (read port :: PortNumber) 0) -- Bind socket to port 10000
    listen sock 2 -- Listen for connections
    msgChan <- newChan
    serverLoop sock msgChan
    close sock

serverLoop :: Socket -> Chan Msg -> IO ()
serverLoop socket msgChan = do
    connection <- accept socket
    forkIO (handleClient connection msgChan) -- Create a new thread for the accepted client
    serverLoop socket msgChan

handleClient :: (Socket, socketAddr) -> Chan Msg -> IO ()
handleClient (socket, _) msgChan = do
    
    handle <- socketToHandle socket ReadWriteMode -- Create a sockethandle for client communication
    username <- hGetLine handle
    password <- hGetLine handle
    hPutStrLn handle "Login successfull"
    clientStateMVar <- newEmptyMVar
    putMVar clientStateMVar (ClientState username False Map.empty)
    forkIO (outLoop handle clientStateMVar msgChan)
    inLoop handle clientStateMVar msgChan
    hClose handle
    close socket

inLoop :: Handle -> MVar ClientState -> Chan Msg -> IO ()
inLoop handle clientStateMVar msgChan = do
    inLine <- hGetLine handle
    clientState <- readMVar clientStateMVar
    putStrLn inLine
    case isCommand inLine clientState of
        Nothing -> do 
            writeChan msgChan (Msg (username clientState) "" inLine)
        Just commandReturn -> do
            hPutStrLn handle (fst commandReturn)
            putMVar clientStateMVar (snd commandReturn)
    clientState <- readMVar clientStateMVar 
    if quit clientState
    then
        return ()
    else inLoop handle clientStateMVar msgChan


outLoop :: Handle -> MVar ClientState -> Chan Msg -> IO ()
outLoop handle clientStateMVar msgChan = do
    
    broadChan <- dupChan msgChan
    recievedMsg <- readChan broadChan
    hPutStrLn handle (sender recievedMsg ++ ": " ++ content recievedMsg)
    clientState <- readMVar clientStateMVar
    if quit clientState
    then
        return()
    else outLoop handle clientStateMVar msgChan
    