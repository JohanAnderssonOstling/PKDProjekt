module ParseInput where

import Network.Socket
import System.IO
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
   users :: [String]
}


isCommand :: String -> ClientState -> Maybe (String, ClientState)
isCommand string clientState
 | head string == '/' = Just (commands (words string) clientState) --input is a command
 | otherwise = Nothing --input is not a command


commands :: [String] -> ClientState -> (String, ClientState)
commands ["/quit"] (ClientState {username=u,quit=False,muted=m}) = 
    ("disconnecting", 
    ClientState {username=u,quit=True,muted=m}) --client quits server
commands ["/mute",username] (ClientState {username=u,quit=q,muted=mUsers}) = 
    ("Muted " ++ username, 
    ClientState {username=u, quit=q, muted=(Map.insert username True mUsers)}) --mutes specified username
commands ["/unmute",username] (ClientState {username=u,quit=q,muted=mUsers}) =
    ("unmuted" ++ username,
    ClientState {username=u, quit=q, muted=(Map.insert username False mUsers)}) --unmutes specified username
commands ["/muted"] (ClientState {username=u,quit=q,muted=mUsers})
 | Map.filter (== True) mUsers == Map.empty = ("No muted users", ClientState {username=u,quit=q,muted=mUsers})
 | otherwise =
    ((concat $ Map.keys (Map.filter (== True) mUsers)),
    ClientState {username=u,quit=q,muted=mUsers}) --displays a list of all muted users
commands ["/commands"] clientState = ("/quit, /mute user, /unmute user, /muted", clientState) --displays a list of available commands
commands _ clientState = ("Unknown command",clientState)







