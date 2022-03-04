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
    (socket, handle) <- connectToServer ip port
    putStrLn "Enter username"
    username <- getLine
    putStrLn "Enter password"
    password <- getLine
    response <- login handle username password
    if (response) == "1"
        then do
            forkID <- forkIO (userInputLoop handle)
            serverInputLoop handle
            hClose handle
        else do
            close socket
            main
    
    close socket
    main

connectToServer host port = do
    putStrLn ("Connecting to host: " ++ host ++ " on port: " ++ port)
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfo
    socket <- socket (addrFamily serverAddr) Stream defaultProtocol --Create client socket
    connect socket (addrAddress serverAddr) --Connect socket to server
    handle <- socketToHandle socket ReadWriteMode
    return (socket, handle)


--login :: Socket -> Handle -> (String, String) -> String
login handle username password = do
    
    hPutStrLn handle username
    hPutStrLn handle password
    --response <- hGetLine handle
    response <- hGetLine handle
    return response
    

serverInputLoop :: Handle -> IO ()
serverInputLoop handle = do
    inLine <- hGetLine handle
    if inLine == "disconnect"
        then do
            putStrLn "Disconnected from server"
        else do
            putStrLn inLine
            serverInputLoop handle

userInputLoop handle = do
    userInput <- getLine
    hPutStrLn handle userInput
    userInputLoop handle
