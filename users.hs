module Users where
    
import System.IO
import Test.HUnit
import qualified Data.Map as Map

type Username = String
type Password = String
type UserMap = Map.Map Username Password

serializeUsers :: UserMap -> String 
serializeUsers users = Map.foldlWithKey getUser "" users

getUser :: String -> Username -> Password -> String
getUser acc username password = acc ++ username ++ " " ++ password ++ "\n"

writeUsers :: UserMap -> IO ()
writeUsers users = do
    file <- openFile "users.txt" WriteMode
    hPutStr file (serializeUsers users)
    hClose file

--------------------------

createUserList :: [String] -> UserMap -> UserMap
createUserList [] map = map
createUserList (x:xs) map = 
    let (name,pass) = (head (words x),last (words x)) 
    in 
        createUserList xs (Map.insert name pass map)

readUsers :: IO UserMap
readUsers = do 
    content <- readFile "users.txt"
    let forceClose = (length content)
    putStrLn ("force close" ++ (show forceClose)) -- fix later
    let users = (createUserList (lines content) Map.empty)
    return users

------------------------

addUser :: Username -> Password -> IO ()
addUser username password = do
    currentUsers <- readUsers
    writeUsers (Map.insert username password currentUsers)
    putStrLn ("User \"" ++ username ++ "\" successfully added")


removeUser :: Username -> IO ()
removeUser username = do
    currentUsers <- readUsers
    writeUsers (Map.delete username currentUsers)
    putStrLn ("User \"" ++ username ++ "\" successfully removed")

clearUsers :: IO ()
clearUsers = do
    writeUsers Map.empty
    putStrLn "All users cleared successfully"


-- ------------------------- test cases (need to get updated)

-- addAndReadUser name pass = do
--     addUser name pass
--     users <- readUsers
--     return users

-- removeAndReadUser name = do
--     removeUser name
--     users <- readUsers
--     return users

-- testAddUser = TestCase $ do 
--     currentUsers <- readUsers
--     let y = currentUsers ++ [User "user1" "pass1"]
--     x <- addAndReadUser "user1" "pass1"
--     assertEqual "error when adding" x y

-- testRemoveUser = TestCase $ do 
--     currentUsers <- readUsers
--     addUser "user2" "pass2"
--     x <- removeAndReadUser "user2"
--     assertEqual "error when removing" x currentUsers

-- -- this will clear all users
-- testClearUsers = TestCase $ do 
--     clearUsers
--     users <- readUsers
--     assertEqual "error when clearing" [] users

-- tests = TestList [TestLabel "testAddUser" testAddUser, TestLabel "testRemoveUser" testRemoveUser, TestLabel "testClearUsers" testClearUsers] 

-- -- use this wrapper to run the tests
-- runTests = runTestTT tests
