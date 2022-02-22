module Client where
import System.IO
import Network.Socket

main :: IO ()
main = do
    connectToServer "127.0.0.1" 14000

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
    line <- hGetLine handle
    putStrLn line
    hPutStrLn handle "Hello from client"
    line <- hGetLine handle
    putStrLn line
    hClose handle
    close socket

