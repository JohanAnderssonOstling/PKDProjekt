module Client where
import System.IO
import Types
import Network.Socket
import Control.Concurrent
import Control.Monad
import System.Environment 

{- startClient
    Gets server and user info from the program input and calls functions for 
    connecting to server and logging in. Close handle and socket after the function
    login has stopped executing.
    SIDE EFFECTS: Closes handle and socket. Writes to program output.
-}
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

{- connectToServer host port
    Connects to host on port and creates a handle for communication with server
    SIDE EFFECTS: Creates socket and handle
    EXAMPLE: connectToServer "127.0.0.1" "5000" == (<socket: 13>,{handle: <socket: 13>})
-}
connectToServer :: String -> String -> IO (Socket, Handle)
connectToServer host port = do
    putStrLn ("Connecting to host: " ++ host ++ " on port: " ++ port)
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfo
    socket <- socket (addrFamily serverAddr) Stream defaultProtocol --Create client socket
    connect socket (addrAddress serverAddr) --Connect socket to server
    handle <- socketToHandle socket ReadWriteMode
    return (socket, handle)

{-  login handle username password
    Writes username and password to server and gets response on whether login
    was successful. If successful it starts two loops that communicate with the server.
    Otherwise it prints the reason the login was not successful.
    SIDE EFFECTS: Writes to server, starts thread, writes to program output.
    PRE: The connection that the handle is based on is active.
-}
login ::  Handle -> Username -> Password -> IO ()
login handle username password = do
    
    hPutStrLn handle username
    hPutStrLn handle password
    response <- hGetLine handle
    if response == "1"
        then do
            clientRunningMVar <- newEmptyMVar
            putMVar clientRunningMVar True
            forkID <- forkIO (userInputLoop handle clientRunningMVar)
            serverInputLoop handle clientRunningMVar
        else do
            errorMsg <- hGetLine handle
            putStrLn errorMsg
    
{- serverInputLoop handle clientRunningMVar
    Gets input from server. If "disconnect" is recieved the loop is terminated.
    Otherwise the recieved line is written to program output.
    PRE: The connection that the handle is based on is active.
    SIDE EFFECTS: Writes to program output and changes MVar.
-}
serverInputLoop :: Handle -> MVar Bool -> IO ()
serverInputLoop handle clientRunningMVar = do
    inLine <- hGetLine handle
    if inLine == "Disconnecting"
        then do
            takeMVar clientRunningMVar
            putMVar clientRunningMVar False
            putStrLn "Disconnected from server"
        else do
            putStrLn inLine
            serverInputLoop handle clientRunningMVar


{-  userInputLoop handle clientRunningMVar
    Gets program input and writes it to server.
    PRE: The connection that the handle is based on is active.
    SIDE EFFECTS: Writes to server
-}
userInputLoop :: Handle -> MVar Bool -> IO ()
userInputLoop handle clientRunningMVar = do
    userInput <- getLine
    unless (userInput == "") $ do
        hPutStrLn handle userInput
    running <- readMVar clientRunningMVar
    when running $ do
        userInputLoop handle clientRunningMVar
