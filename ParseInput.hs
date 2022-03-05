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

data ClientState = ClientState {
    username :: Username,
    quit :: Bool,
    muted :: Set Username
} deriving (Show)

data ServerState = ServerState {
   users :: Map Username UserPassword,
   onlineUsers :: Set Username
} deriving (Show)


isCommand :: String -> ClientState -> ServerState -> Maybe (String, ClientState, ServerState)
isCommand string clientState serverState
 | head string == '/' = Just (commands (words string) clientState serverState) --input is a command
 | otherwise = Nothing --input is not a command


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
    ("unmuted" ++ username,
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



--Lägg till dokumentation till funktionerna
--Lägg till ett fall i mute och unmute ifall användaren man försöker muta inte finns



