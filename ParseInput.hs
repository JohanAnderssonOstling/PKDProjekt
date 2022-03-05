module ParseInput where

import Network.Socket
import System.IO
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
type Username = String
type UserPassword = String

{-ClientState
  username = the clients username
  quit = false when the client is online and when it becomes true the client is disconnected
  muted = a set of usernames of users who have been muted
-}
data ClientState = ClientState {
    username :: Username,
    quit :: Bool,
    muted :: Set Username
} deriving (Show)

{-ServerState
  users = a map of usernames and passwords of the clients registered on the server
  onlineUsers = a set of usernames of users currently online on the server
-}
data ServerState = ServerState {
   users :: Map Username UserPassword,
   onlineUsers :: Set Username
} deriving (Show)

{-isCommand
  checks if input string is a command and calls commands if true
  RETURNS: Maybe (String, ClientState, ServerState)
  EXAMPLES: isCommand "/quit" ClientState {username="friend",quit=False,muted=Set.fromList ["mate","buddy"] } ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]} == Just ("disconnecting",ClientState {username = "emil", quit = True, muted = fromList ["buddy","mate"]},ServerState {users = fromList [("buddy","qwerty"),("emil","zxc"),("mate","123")], onlineUsers = fromList ["buddy","emil","mate"]})
            isCommand "hello" = Nothing
-}
isCommand :: String -> ClientState -> ServerState -> Maybe (String, ClientState, ServerState)
isCommand string clientState serverState
 | head string == '/' = Just (commands (words string) clientState serverState) --input is a command
 | otherwise = Nothing --input is not a command

{-commands
  Modifies clientstate and serverstate depending on the strings in the list
  RETURNS: (String, ClientState, ServerState)
-}
commands :: [String] -> ClientState -> ServerState -> (String, ClientState, ServerState)
commands ["/quit"] (ClientState {username=u,quit=False,muted=m}) serverState = 
    ("disconnecting", 
    ClientState {username=u,quit=True,muted=m},
    serverState) --client quits server
commands ["/mute",username] (ClientState {username=u,quit=q,muted=mutedUsers}) serverState
 | Set.member username (onlineUsers serverState) == True = 
    ("Muted " ++ username, 
    ClientState {username=u, quit=q, muted=Set.insert username mutedUsers},
    serverState) --mutes specified username
 | otherwise = ("User not found",
 (ClientState {username=u,quit=q,muted=mutedUsers}),
 serverState) --user is not on the server
commands ["/unmute",username] (ClientState {username=u,quit=q,muted=mutedUsers}) serverState 
 | Set.member username mutedUsers == True =
    ("unmuted " ++ username,
    ClientState {username=u, quit=q, muted=(Set.delete username mutedUsers)},
    serverState) --unmutes specified username
 | otherwise = 
     ("User is not muted",
     (ClientState {username=u,quit=q,muted=mutedUsers}),
     serverState)
commands ["/muted"] (ClientState {username=u,quit=q,muted=mutedUsers}) serverState
 | mutedUsers == Set.empty = 
    ("No muted users", 
    ClientState {username=u,quit=q,muted=mutedUsers},
    serverState)
 | otherwise =
    ((intercalate ", " $ Set.toList mutedUsers),
    ClientState {username=u,quit=q,muted=mutedUsers},
    serverState) --displays a list of all muted users
commands ["/users"] clientState serverState = 
    ((intercalate ", " $ Set.toList (onlineUsers serverState)),
    clientState,
    serverState)
commands ["/commands"] clientState serverState = ("/quit, /mute user, /unmute user, /muted, /users", clientState, serverState) --displays a list of available commands
commands _ clientState serverState = ("Unknown command", clientState, serverState) --unknown command



















--Testing

clientTester = ClientState {username="friend",quit=False,muted=Set.fromList ["mate","buddy"] }
clientTester2 = ClientState {username="friend",quit=False,muted=Set.empty }

serverTester = ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}


