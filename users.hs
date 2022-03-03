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

clearUsers = do
    writeFile "users.txt" ""
    putStrLn "All users cleared successfully"


------------------------- test cases

addAndReadUser name pass = do
    addUser name pass
    users <- readUsers
    return users

test1 = TestCase $ do 
    currentUsers <- readUsers
    let y = currentUsers ++ [User "user1" "pass1"]
    x <- addAndReadUser "user1" "pass1"
    assertEqual "user did not att successfully" x y

