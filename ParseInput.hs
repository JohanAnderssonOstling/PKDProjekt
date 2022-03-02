module ParseInput where

import Network.Socket
import System.IO
import Server
import qualified Data.Map as Map

isCommand :: String -> ClientState -> ClientState
isCommand string clientState
 | head string == '/' = commands (words string) clientState --input is a command
 | otherwise = clientState --input is not a command

commands :: [String] -> ClientState -> ClientState
commands ["/quit"] ClientState {quit=False,muted=x} = ClientState {quit=True,muted=x} --client quits server
commands ["/mute",username] (ClientState {quit=x,muted=mUsers}) = ClientState {quit=x, muted=(Map.insert username True mUsers)} --mutes specified username
commands _ clientState = clientState


















