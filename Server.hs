module Server where
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Network.Socket
import Data.Map (Map)
import qualified Data.Map       as Map
import Control.Monad
import System.Environment
import ParseInput
import Types
import Users
import Data.Set as Set


{- Msg
    sender = the username of the sender
    content = the body of the message
-}
data Msg = Msg {
    sender :: Username,
    content :: String
}
{- main
    Gets port and creates MVar for storing whether the server should continue to run.
    Then calls startServer.
    SIDE EFFECTS: Creates and writes to MVar
-}
main :: IO ()
main = do
    putStrLn "Enter port"
    port <- getLine
    serverRunningMVar <- newEmptyMVar
    putMVar serverRunningMVar True
    startServer port serverRunningMVar
    
{-
    Setups the server socket and mechanism for storing state between threads and
    reads the registered users from file
    PRE: The port is not in use
    SIDE EFFECTS: Starts a server socket, message channel, reads users from file
    , and creates an MVar for the the server state
    EXAMPLE: startServer 25000
-}
startServer :: String -> MVar Bool -> IO ()
startServer port serverRunningMVar = do
    putStrLn ("Starting server on port: " ++ port)
    sock <- socket AF_INET Stream 0 -- Create a socket
    bind sock (SockAddrInet (read port :: PortNumber) 0) -- Bind socket to the port
    listen sock 2 -- Listen for connections
    msgChan <- newChan
    serverStateMVar <- newEmptyMVar
    users <- readUsers
    putMVar serverStateMVar (ServerState users Set.empty)
    serverLoop sock msgChan serverStateMVar serverRunningMVar
    close sock

{- stopServer serverRunningMVar
    SIDE EFFECTS: Changes value of server running MVar to false, causing
    loop in other thread to stop
-}
stopServer :: MVar Bool -> IO ()
stopServer serverRunningMVar = do
    takeMVar serverRunningMVar
    putMVar serverRunningMVar False

{- serverLoop socket msgChan serverStateMVar
    Loop that accepts incoming connections and starts a thread for each one
    SIDE EFFECTS: Creates sockets and starts threads
-}
serverLoop :: Socket -> Chan Msg -> MVar ServerState -> MVar Bool -> IO ()
serverLoop socket msgChan serverStateMVar serverRunningMVar = do
    connection <- accept socket
    forkIO (authenticateClient (Prelude.fst connection) msgChan serverStateMVar) -- Create a new thread for the accepted client
    running <- readMVar serverRunningMVar
    when running $ do
        serverLoop socket msgChan serverStateMVar serverRunningMVar

{- registerUser serverState username password
    RETURNS: ServerState with the registered user inserted in users and onlineUsers
-}
registerUser :: ServerState -> Username -> Password -> ServerState
registerUser serverState username password =
    ServerState newUsers newOnlineUsers
    where 
        newUsers = Map.insert username password (users serverState)
        newOnlineUsers = Set.insert username (onlineUsers serverState)

{- loginUser serverState username
    RETURNS: ServerState with username inserted in onlineUsers
-}
loginUser :: ServerState -> Username -> ServerState
loginUser serverState username =
    ServerState (users serverState) newOnlineUsers
    where
        newOnlineUsers = Set.insert username (onlineUsers serverState)

{- authenticateClient socket msgChan serverStateMVar
    Gets username and password of user from client. If user does not exist,
    a new user is created. If client exists and password is correct, client
    is logged in. If password is incorrect or client already is connected then the
    the login is refused.
    SIDE EFFECTS: Writes registered users to file, writes to client and changes the MVar
    for server state.
-}
authenticateClient :: Socket -> Chan Msg -> MVar ServerState -> IO ()
authenticateClient socket msgChan serverStateMVar = do
    handle <- socketToHandle socket ReadWriteMode 
    hPutStrLn handle "Connection established"
    username <- hGetLine handle
    password <- hGetLine handle
    serverState <- takeMVar serverStateMVar
 
    if Map.notMember username (users serverState)
        then do -- User does not exist, register and then login
            putMVar serverStateMVar (registerUser serverState username password)
            addUser username password
            hPutStrLn handle "1"
            hPutStrLn handle ("Registered user with username: " ++ username ++ " and password: " ++ password)
            setupClient socket handle msgChan serverStateMVar username
        else do -- User exists
            case Map.lookup username (users serverState) of
                Just lookupPassword ->
                    if password == lookupPassword
                        then do -- The password is correct
                            if Set.notMember username (onlineUsers serverState)
                                then do -- The user is not already logged in
                                    putMVar serverStateMVar (loginUser serverState username)
                                    hPutStrLn handle "1"
                                    hPutStrLn handle ("Login succesful with username" ++ username)
                                    setupClient socket handle msgChan serverStateMVar username
                                else do -- The user is already logged in
                                    putMVar serverStateMVar serverState
                                    hPutStrLn handle "0"
                                    hPutStrLn handle (username ++ "already logged in")
                        else do -- The password is incorrect
                            putMVar serverStateMVar serverState
                            hPutStrLn handle "0"
                            hPutStrLn handle ("Wrong password: " ++ password)

{- setupClient socket handle msgChan serverStateMVar username
    Setups state sharing mechanisms and starts loops that accept input from client
    and the message channel. Closes handle and socket after the loops have been
    stopped.
    SIDE EFFECTS: Create MVar, starts thread, closes handle and socket
-}
setupClient :: Socket -> Handle -> Chan Msg -> MVar ServerState -> Username -> IO ()
setupClient socket handle msgChan serverStateMVar username = do

    clientStateMVar <- newEmptyMVar
    putMVar clientStateMVar (ClientState username False Set.empty)
    forkIO (outputLoop handle msgChan clientStateMVar )
    inputLoop handle msgChan serverStateMVar clientStateMVar
    hClose handle
    close socket

{- inputLoop handle  msgChan serverStateMVar clientStateMVar
    Gets input from the client, changes state and sends response to client if command
    , otherwise, writes the input to the message channel. Stops when the client
    disconnects
    SIDE EFFECTS: Writes to and reads from client, writes to the MVar for client 
    state and the message channel
-}
inputLoop :: Handle -> Chan Msg -> MVar ServerState -> MVar ClientState -> IO ()
inputLoop handle  msgChan serverStateMVar clientStateMVar= do
    inLine <- hGetLine handle
    clientState <- takeMVar clientStateMVar
    serverState <- readMVar serverStateMVar
    putStrLn inLine
    case isCommand inLine clientState serverState of
        Nothing -> do
            putMVar clientStateMVar clientState
            writeChan msgChan (Msg (username clientState) inLine)
        Just commandReturn -> do
            hPutStrLn handle (Server.fst commandReturn)
            putMVar clientStateMVar (Server.snd commandReturn)
            --putMVar serverStateMVar (Server.thrd commandReturn)
    clientState <- readMVar clientStateMVar
    if quit clientState
        then do 
            serverState <- takeMVar serverStateMVar
            let newServerState = ServerState (users serverState) (Set.delete (username clientState) (onlineUsers serverState))
            putMVar serverStateMVar newServerState
        else do
            inputLoop handle msgChan serverStateMVar clientStateMVar


{- outputLoop handle msgChan clientStateMVar
    Duplicates the message channel and then reads a message from the duplicated
    channel. If the message is not muted, the body of the message is written to the client.
    Stops when the client disconnects.
    SIDE EFFECTS: Writes to client
-}
outputLoop :: Handle -> Chan Msg -> MVar ClientState -> IO ()
outputLoop handle msgChan clientStateMVar = do

    broadChan <- dupChan msgChan
    recievedMsg <- readChan broadChan
    clientState <- readMVar clientStateMVar
    when (Set.notMember (sender recievedMsg) (muted clientState))
        $ do
            hPutStrLn handle (sender recievedMsg ++ ": " ++ content recievedMsg)

    unless(quit clientState)
        $ outputLoop handle msgChan clientStateMVar

fst (a,b, c) = a
snd (a, b, c) = b
thrd (a,b,c) = c