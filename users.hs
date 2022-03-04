import System.IO
import Test.HUnit

data User = User {
      username :: String
    , password :: String
    } deriving (Show, Eq)

serializeUsers :: [User] -> String 
serializeUsers users = foldl getUser "" users

getUser :: String -> User -> String
getUser acc (User username password) = acc ++ username ++ " " ++ password ++ "\n"

writeUsers :: [User] -> IO ()
writeUsers users = do
    file <- openFile "users.txt" WriteMode
    hPutStr file (serializeUsers users)
    hClose file

--------------------------

userList :: [String] -> [User]
userList [] = []
userList (x:xs) = [User (head (words x)) (last (words x))] ++ userList xs

readUsers :: IO [User]
readUsers = do 
    content <- readFile "users.txt"
    let forceClose = (length content)
    putStrLn ("force close" ++ (show forceClose)) -- fix later
    let users = (userList (lines content))
    return users

------------------------

addUser :: String -> String -> IO ()
addUser username password = do
    currentUsers <- readUsers
    writeUsers (currentUsers ++ [User username password])
    putStrLn ("User \"" ++ username ++ "\" successfully added")


removeUser' :: String -> [User] -> [User] -> [User]
removeUser' _ [] acc = acc
removeUser' target ((User name pass):xs) acc
    | name == target = removeUser' target xs acc
    | otherwise = removeUser' target xs (acc ++ [User name pass])

removeUser :: String -> IO ()
removeUser username = do
    currentUsers <- readUsers
    writeUsers (removeUser' username currentUsers [])
    putStrLn ("User \"" ++ username ++ "\" successfully removed")

clearUsers :: IO ()
clearUsers = do
    writeFile "users.txt" ""
    putStrLn "All users cleared successfully"


------------------------- test cases

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
    let y = currentUsers ++ [User "user1" "pass1"]
    x <- addAndReadUser "user1" "pass1"
    assertEqual "error when adding" x y

testRemoveUser = TestCase $ do 
    currentUsers <- readUsers
    addUser "user2" "pass2"
    x <- removeAndReadUser "user2"
    assertEqual "error when removing" x currentUsers

-- this will clear all users
testClearUsers = TestCase $ do 
    clearUsers
    users <- readUsers
    assertEqual "error when clearing" [] users

tests = TestList [TestLabel "testAddUser" testAddUser, TestLabel "testRemoveUser" testRemoveUser, TestLabel "testClearUsers" testClearUsers] 