module Client where
import System.IO
import Network.Socket
import Control.Concurrent
main :: IO ()
main = do
    connectToServer "127.0.0.1" 18000

connectToServer :: String -> Int -> IO ()
connectToServer host port = do
    putStrLn ("Connecting to host: " ++ host ++ " on port: " ++ show(port))
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    socket <- socket (addrFamily serverAddr) Stream defaultProtocol --Create client socket
    connect socket (addrAddress serverAddr) --Connect socket to server
    sendEcho socket

sendEcho :: Socket -> IO ()
sendEcho socket = do
    handle <- socketToHandle socket ReadWriteMode
    forkIO (inLoop handle)
    outLoop handle
    hClose handle
    close socket

inLoop :: Handle -> IO ()
inLoop handle = do
    inLine <- hGetLine handle
    putStrLn inLine
    inLoop handle

outLoop :: Handle -> IO ()
outLoop handle = do
    outLine <- getLine
    hPutStrLn handle outLine
    outLoop handle
