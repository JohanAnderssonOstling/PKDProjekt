module ParseInput where

import Network.Socket
import System.IO
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
type Username = String
type UserPassword = String

data ClientState = ClientState {
    username :: Username,
    quit :: Bool,
    muted :: Map Username Bool
} deriving (Show)

data ServerState = ServerState {
   users :: [Username]
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
commands ["/mute",username] (ClientState {username=u,quit=q,muted=mutedUsers}) serverState = 
    ("Muted " ++ username, 
    ClientState {username=u, quit=q, muted=(Map.insert username True mutedUsers)},
    serverState) --mutes specified username
commands ["/unmute",username] (ClientState {username=u,quit=q,muted=mutedUsers}) serverState =
    ("unmuted" ++ username,
    ClientState {username=u, quit=q, muted=(Map.insert username False mutedUsers)},
    serverState) --unmutes specified username
commands ["/muted"] (ClientState {username=u,quit=q,muted=mutedUsers}) serverState
 | Map.filter (== True) mutedUsers == Map.empty = 
    ("No muted users", 
    ClientState {username=u,quit=q,muted=mutedUsers},
    serverState)
 | otherwise =
    ((intercalate ", " (Map.keys (Map.filter (== True) mutedUsers))),
    ClientState {username=u,quit=q,muted=mutedUsers},
    serverState) --displays a list of all muted users
commands ["/users"] clientState serverState = 
    ((intercalate ", " $ users serverState),
    clientState,
    serverState)
commands ["/commands"] clientState serverState = ("/quit, /mute user, /unmute user, /muted, /users", clientState, serverState) --displays a list of available commands
commands _ clientState serverState = ("Unknown command", clientState, serverState)







