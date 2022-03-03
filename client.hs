module Client where
import System.IO
import Network.Socket
import Control.Concurrent
import System.Environment 
main :: IO ()
main = do
    putStrLn "Enter ip"
    ip <- getLine
    putStrLn "Enter port"
    port <- getLine
    connectToServer ip port
    main

connectToServer :: String -> String -> IO ()
connectToServer host port = do
    putStrLn ("Connecting to host: " ++ host ++ " on port: " ++ port)
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfo
    socket <- socket (addrFamily serverAddr) Stream defaultProtocol --Create client socket
    connect socket (addrAddress serverAddr) --Connect socket to server
    userInfo <- getUserInfo
    sendEcho socket userInfo


--getUserInfo :: IO -> (String, String)
getUserInfo = do
    putStrLn "Enter username"
    username <- getLine
    putStrLn "Enter password"
    password <- getLine
    return (username, password)

sendEcho :: Socket -> (String, String) -> IO ()
sendEcho socket (username, password) = do
    handle <- socketToHandle socket ReadWriteMode
    
    hPutStrLn handle username
    hPutStrLn handle password
    response <- hGetLine handle
    putStrLn response
    forkID <- forkIO (outLoop handle)
    inLoop handle
    killThread forkID
    hClose handle
    close socket

inLoop :: Handle -> IO ()
inLoop handle = do
    inLine <- hGetLine handle
    if inLine == "disconnect"
        then do
            putStrLn "Disconnected from server"
        else do
            putStrLn inLine
            inLoop handle

outLoop :: Handle -> IO ()
outLoop handle = do
    outLine <- getLine

    hPutStrLn handle outLine
    outLoop handle
