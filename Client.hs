module Client where
import System.IO
import Types
import Network.Socket
import Control.Concurrent
import System.Environment 
startClient :: IO ()
startClient = do
    putStrLn "Enter host"
    host <- getLine
    putStrLn "Enter port"
    port <- getLine
    (socket, handle) <- connectToServer host port
    connectionResponse <- hGetLine handle
    putStrLn connectionResponse
    putStrLn "Enter username"
    username <- getLine
    putStrLn "Enter password"
    password <- getLine
    login handle username password
    hClose handle
    close socket
    startClient


connectToServer :: String -> String -> IO (Socket, Handle)
connectToServer host port = do
    putStrLn ("Connecting to host: " ++ host ++ " on port: " ++ port)
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfo
    socket <- socket (addrFamily serverAddr) Stream defaultProtocol --Create client socket
    connect socket (addrAddress serverAddr) --Connect socket to server
    handle <- socketToHandle socket ReadWriteMode
    return (socket, handle)


login ::  Handle -> Username -> Password -> IO ()
login handle username password = do
    
    hPutStrLn handle username
    hPutStrLn handle password
    response <- hGetLine handle
    if response == "1"
        then do
            forkID <- forkIO (userInputLoop handle)
            serverInputLoop handle
            hClose handle
        else do
            errorMsg <- hGetLine handle
            putStrLn errorMsg
    

serverInputLoop :: Handle -> IO ()
serverInputLoop handle = do
    inLine <- hGetLine handle
    if inLine == "disconnect"
        then do
            putStrLn "Disconnected from server"
        else do
            putStrLn inLine
            serverInputLoop handle

userInputLoop :: Handle -> IO ()
userInputLoop handle = do
    userInput <- getLine
    hPutStrLn handle userInput
    userInputLoop handle
