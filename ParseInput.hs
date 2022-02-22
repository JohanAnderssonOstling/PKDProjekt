module ParseInput(parseInput) where

import System.Exit --in case it's needed







parseInput :: String -> IO ()
parseInput input
 | (head input) == '/' = commands (tail input) --input is a command
 | otherwise = undefined --input is a text message



commands :: String -> IO ()
commands "quit" = undefined --leave server
commands "mute username" = undefined --mutes a certain user
commands _ = undefined --unknown command error


















