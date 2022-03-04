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

createUserMap :: [String] -> UserMap -> UserMap
createUserMap [] map = map
createUserMap (x:xs) map = 
    let (name,pass) = (head (words x),last (words x)) 
    in 
        createUserMap xs (Map.insert name pass map)

readUsers :: IO UserMap
readUsers = do 
    content <- readFile "users.txt"
    let forceClose = (length content)
    putStrLn ("----------------------------------" ++ (show forceClose) ++ "") -- fix later
    let users = (createUserMap (lines content) Map.empty)
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


getPassword :: Username -> IO (Maybe Password)
getPassword user = do
    currentUsers <- readUsers
    return (Map.lookup user currentUsers)


------------------------- test cases (need to get updated)

addAndReadUser name pass = do
    addUser name pass
    users <- readUsers
    return users

removeAndReadUser name = do
    removeUser name
    users <- readUsers
    return users

testAddUser = TestCase $ do 
    currentUsers <- readUsers
    let y = Map.insert "user1" "pass1" currentUsers
    x <- addAndReadUser "user1" "pass1"
    assertEqual "error when adding" x y

testRemoveUser = TestCase $ do 
    currentUsers <- readUsers
    addUser "user2" "pass2"
    x <- removeAndReadUser "user2"
    assertEqual "error when removing" x currentUsers

-- this will clear all users
testClearUsers = TestCase $ do 
    usersBeforeClearing <- readUsers
    clearUsers
    users <- readUsers
    assertEqual "error when clearing" Map.empty users
    writeUsers usersBeforeClearing -- restores the users.txt file to the state it was before the test

tests = TestList [TestLabel "testAddUser" testAddUser, TestLabel "testRemoveUser" testRemoveUser, TestLabel "testClearUsers" testClearUsers] 

-- use this wrapper to run the tests
runTests = runTestTT tests