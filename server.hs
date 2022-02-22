module Server where
import System.IO
import Control.Concurrent
import Network.Socket


main :: IO ()
main = do
    putStrLn "Creating socket"
    sock <- socket AF_INET Stream 0 -- Create a socket
    setSocketOption sock ReuseAddr 1 -- Set socket reuse on
    bind sock (SockAddrInet 14000 0) -- Bind socket to port 10000
    listen sock 2 -- Listen for connections
    serverLoop sock

serverLoop :: Socket -> IO ()
serverLoop socket = do
    putStrLn "Accepting clients"
    connection <- accept socket
    forkIO (handleClient connection) -- Create a new thread for the accepted client
    serverLoop socket

handleClient :: (Socket, socketAddr) -> IO ()
handleClient (socket, _) = do
    putStrLn "Client connected"
    handle <- socketToHandle socket ReadWriteMode -- Create a sockethandle for client communication
    hPutStrLn handle "Hello World!" -- Send a message to the client
    line <- hGetLine handle -- Get message from client
    putStrLn ("Message from client: " ++ line)
    hPutStrLn handle ("Echo: " ++ line)
    hClose handle
    close socket