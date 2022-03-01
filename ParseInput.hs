module ParseInput(parseInput) where

import System.Exit --in case it's needed







parseInput :: String -> IO ()
parseInput input
 | (head input) == '/' = commands (tail input) --input is a command
 | otherwise = do                              --input is a text message
    putStrLn input
    newInput <- getLine
    parseInput newInput
 


commands :: String -> IO ()
commands "quit" = undefined          --leave server
commands "mute username" = undefined --mutes a certain user
commands _ = undefined               --unknown command error


















