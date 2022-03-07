import Server
import Client
import Test.HUnit
import Network.Socket
import System.IO
import Control.Concurrent.STM
import Control.Concurrent

-------------Testing-------------

testConnection port = TestCase $ do 
    serverRunningMVar <- newEmptyMVar
    putMVar serverRunningMVar True
    startServer port serverRunningMVar
    (socket, handle) <- connectToServer "127.0.0.1" port
    response <- hGetLine handle
    stopServer serverRunningMVar
    assertEqual "error when establishing connection" "Connection established" response

testLogin port = TestCase $ do
    serverRunningMVar <- newEmptyMVar
    putMVar serverRunningMVar True
    startServer port serverRunningMVar

    (socket, handle) <- connectToServer "127.0.0.1" port
    response <- hGetLine handle
    hPutStrLn handle "test1user"
    hPutStrLn handle "test1pass"
    loginResponse <- hGetLine handle
    assertEqual "error when logging in" "1" loginResponse

testMsg port = TestCase $ do 
    serverRunningMVar <- newEmptyMVar
    putMVar serverRunningMVar True
    startServer port serverRunningMVar

    (socket, handle1) <- connectToServer "127.0.0.1" port
    response <- hGetLine handle1
    hPutStrLn handle1 "test1user"
    hPutStrLn handle1 "test1pass"
    loginResponse1 <- hGetLine handle1
    loginMessage1 <- hGetLine handle1

    (socket, handle2) <- connectToServer "127.0.0.1" port
    response <- hGetLine handle1
    hPutStrLn handle2 "test2user"
    hPutStrLn handle2 "test2pass"
    loginResponse2 <- hGetLine handle2
    loginMessage2 <- hGetLine handle2

    hPutStrLn handle1 "Test message"
    recievedMsg <- hGetLine handle2
    assertEqual "error when sending and recieving message" "test1user: Test message" recievedMsg


tests port = TestList [TestLabel "testConnection" (testConnection (show port)),
                        TestLabel "testLogin" (testLogin (show (port + 1))) ,
                        TestLabel "testMessage" (testMsg (show (port + 2)))]

runTests port = runTestTT (tests port)