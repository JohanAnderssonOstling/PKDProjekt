module Users where
    
import System.IO
import Test.HUnit
import qualified Data.Map as Map
import Types

{- serializeUsers users
    A function which loops over a map of users and puts them in the correct string format
    RETURNS: A string with a space separating each username and password and a newline separating
    each user.
    EXAMPLES: serializeUsers (Map.fromList [("user1","pass1"),("user2","pass2")]) == "user1 pass1\nuser2 pass2\n"
-}
serializeUsers :: UserMap -> String 
serializeUsers users = Map.foldlWithKey getUserString "" users

{- getUserString acc username password
    Helper function for serializeUsers
    RETURNS: acc ++ username ++ " " ++ password ++ "\n"
    EXAMPLES: getUserString "" "user1" "pass1" == "user1 pass1\n"
-}
getUserString :: String -> Username -> Password -> String
getUserString acc username password = acc ++ username ++ " " ++ password ++ "\n"

{- writeUsers users
    Takes a map of users and writes them to users.txt
    SIDE EFFECTS: Overwrites users.txt with each user on a separate line
    and the username and password separated by a space. Screen output
-}
writeUsers :: UserMap -> IO ()
writeUsers users = do
    file <- openFile "users.txt" WriteMode
    hPutStr file (serializeUsers users)
    hClose file

--------------------------

{- createUserMap string acc
    Creates a map of users from a string where each user is separated by a newline character
    and the username and password of each user is separated by a space. 
    RETURNS: A map of users in the string
    PRECONDITION: Each user should be separated by a newline character and the username and password of each user should be separated by a space. 
    EXAMPLES: createUserMap "user1 pass1\nuser2 pass2" == fromList [("user1","pass1"),("user2","pass2")]
              createUserMap "user1 pass1 user2 pass2" == fromList [("user1","pass2")]
-}
createUserMap :: String -> UserMap
createUserMap users = createUserMap' (lines users) Map.empty

{- createUserMap' list acc
    Creates a map of users from a list where each element in the list is a user on the format
    "name password"
    RETURNS: A map of users in the list, with acc as the starting map
    PRECONDITION: Each element in the list should have the format "username password" for the expected result. The acc should also be Map.empty
    EXAMPLES: createUserMap' ["user1 pass1","user2 pass2"] Map.empty == fromList [("user1","pass1"),("user2","pass2")]
              createUserMap' ["user1 pass1","user2pass2"] Map.empty == fromList [("user1","pass1"),("user2pass2","user2pass2")]
-}
createUserMap' :: [String] -> UserMap -> UserMap
createUserMap' [] map = map
createUserMap' (x:xs) map = 
    let (name,pass) = (head (words x),last (words x)) 
    in 
        createUserMap' xs (Map.insert name pass map)

{- readUsers 
    Reads all the users from users.txt
    RETURNS: The UserMap of the users in the file
    SIDE EFFECTS: Reads from users.txt, screen output
-}
readUsers :: IO UserMap
readUsers = do 
    content <- readFile "users.txt"
    let forceClose = (length content)
    putStrLn ("----------------------------------" ++ (show forceClose) ++ "") -- fix later
    let users = (createUserMap content)
    return users

------------------------

{- addUser username password
    Adds a user to the users.txt file
    SIDE EFFECTS: Writes a user to the users.txt file, screen output
    PRECONDITION: The username or password should not contain a space
-}
addUser :: Username -> Password -> IO ()
addUser username password = do
    currentUsers <- readUsers
    writeUsers (Map.insert username password currentUsers)
    putStrLn ("User \"" ++ username ++ "\" successfully added")


{- removeUser username
    Searches for a user in the users.txt file and removes it if it exists, does nothing if it doesnt exist
    SIDE EFFECTS: Removes a user from the users.txt file, screen output
-}
removeUser :: Username -> IO ()
removeUser username = do
    currentUsers <- readUsers
    writeUsers (Map.delete username currentUsers)
    putStrLn ("User \"" ++ username ++ "\" successfully removed")

{- clearUsers
    Clears all the users from the users.txt file
    SIDE EFFECTS: Clears all content from users.txt, screen output
-}
clearUsers :: IO ()
clearUsers = do
    writeUsers Map.empty
    putStrLn "All users cleared successfully"

{- getPassword username
    Searches for a username in the users.txt file
    RETURNS: A Maybe of the password if the user exists, else Nothing
    SIDE EFFECTS: Reads from users.txt, screen output
-}
getPassword :: Username -> IO (Maybe Password)
getPassword user = do
    currentUsers <- readUsers
    return (Map.lookup user currentUsers)


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
