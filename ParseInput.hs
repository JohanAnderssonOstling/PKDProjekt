module ParseInput where

import Network.Socket
import System.IO
import Server
import qualified Data.Map as Map

isCommand :: String -> ClientState -> Maybe (String, ClientState)
isCommand string clientState
 | head string == '/' = Just (commands (words string) clientState) --input is a command
 | otherwise = Nothing --input is not a command

commands :: [String] -> ClientState -> (String, ClientState)
commands ["/quit"] ClientState {quit=False,muted=x} = ("Quitting current server", ClientState {quit=True,muted=x}) --client quits server
commands ["/mute",username] (ClientState {quit=x,muted=mUsers} = ("Muted " ++ username, ClientState {quit=x, muted=(Map.insert username True mUsers)} ) --mutes specified username
commands _ clientState = ("Unknown command",clientState)


















