import System.IO
import Data.Text

data User = User {
      username :: String
    , password :: String
    } deriving (Show)

serializeUsers :: [User] -> String 
serializeUsers users = Prelude.foldl getUser "" users

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
userList (x:xs) = [User (Prelude.head (Prelude.words x)) (Prelude.last (Prelude.words x))] ++ userList xs

readUsers :: IO ()
readUsers = do 
    content <- readFile "users.txt"
    let users = (userList (Prelude.lines content))
    print users
