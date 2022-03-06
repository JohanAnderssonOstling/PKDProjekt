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
import Types
import Users
import Data.Set as Set
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
    serverStateMVar <- newEmptyMVar
    putMVar serverStateMVar (ServerState Map.empty Set.empty)
    serverLoop sock msgChan serverStateMVar
    close sock

serverLoop :: Socket -> Chan Msg -> MVar ServerState -> IO ()
serverLoop socket msgChan serverStateMVar = do
    connection <- accept socket
    forkIO (handleClient connection msgChan serverStateMVar) -- Create a new thread for the accepted client
    serverLoop socket msgChan serverStateMVar

--login handle = do 
  --  username <- hGetLine handle
    --password <- hGetLine handle
    --case getPassword username of
      --  Nothing -> do 
        --    addUser username password
         --   return username
        --Just userPassword -> do
          --  if userPassword == password
            --    then
              --      username
            --else
              --  Nothing


handleClient :: (Socket, socketAddr) -> Chan Msg -> MVar ServerState -> IO ()
handleClient (socket, _) msgChan serverStateMVar = do
    
    handle <- socketToHandle socket ReadWriteMode -- Create a sockethandle for client communication
    username <- hGetLine handle
    password <- hGetLine handle
    hPutStrLn handle "1"
            
    clientStateMVar <- newEmptyMVar
    putMVar clientStateMVar (ClientState username False Set.empty)
    forkIO (outLoop handle msgChan clientStateMVar )
    inLoop handle msgChan serverStateMVar clientStateMVar
    hClose handle
    close socket

inLoop :: Handle -> Chan Msg -> MVar ServerState -> MVar ClientState -> IO ()
inLoop handle  msgChan serverStateMVar clientStateMVar= do
    inLine <- hGetLine handle
    clientState <- readMVar clientStateMVar
    serverState <- readMVar serverStateMVar
    putStrLn inLine
    case isCommand inLine clientState serverState of
        Nothing -> do 
            writeChan msgChan (Msg (username clientState) "" inLine)
        Just commandReturn -> do
            hPutStrLn handle (Server.fst commandReturn)
            putMVar clientStateMVar (Server.snd commandReturn)
            putMVar serverStateMVar (thrd commandReturn)
    clientState <- readMVar clientStateMVar 
    if quit clientState
    then
        return ()
    else inLoop handle msgChan serverStateMVar clientStateMVar


outLoop :: Handle -> Chan Msg -> MVar ClientState -> IO ()
outLoop handle msgChan clientStateMVar = do
    
    broadChan <- dupChan msgChan
    recievedMsg <- readChan broadChan
    clientState <- readMVar clientStateMVar
    hPutStrLn handle (sender recievedMsg ++ ": " ++ content recievedMsg)
    
    if quit clientState
    then
        return()
    else outLoop handle msgChan clientStateMVar 

fst (a,b, c) = a
snd (a, b, c) = b
thrd (a,b,c) = c