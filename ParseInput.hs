module ParseInput where

import Types
import Test.HUnit
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


{-ClientState
  username = the clients username
  quit = false when the client is online and when it becomes true the client is disconnected
  muted = a set of usernames of users who have been muted
-}
data ClientState = ClientState {
    username :: Username,
    quit :: Bool,
    muted :: Set Username
} deriving (Show,Eq) --for testing

{-ServerState
  users = a map of usernames and passwords of the clients registered on the server
  onlineUsers = a set of usernames of users currently online on the server
-}
data ServerState = ServerState {
   users :: Map Username Password,
   onlineUsers :: Set Username
} deriving (Show,Eq) --for testing


{-isCommand
  Checks if input string is a command and calls commands if true
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
  EXAMPLES:commands ["/quit"] ClientState {username="friend",quit=False,muted=Set.fromList ["mate","buddy"] } ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]} == ("disconnecting",ClientState {username = "emil", quit = True, muted = fromList ["buddy","mate"]},ServerState {users = fromList [("buddy","qwerty"),("emil","zxc"),("mate","123")], onlineUsers = fromList ["buddy","emil","mate"]})
-}
commands :: [String] -> ClientState -> ServerState -> (String, ClientState, ServerState)
commands ["/quit"] (ClientState {username=u,quit=False,muted=m}) serverState = 
    ("Disconnecting", 
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
    ("Unmuted " ++ username,
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













-------------Testing-------------


--quit command
testQuit = TestCase $ assertEqual "For /quit" (Just ("Disconnecting",(ClientState {username="friend",quit=True,muted=Set.empty }), (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))) (isCommand "/quit" (ClientState {username="friend",quit=False,muted=Set.empty }) (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))

--mute command
testMute = TestCase $ assertEqual "For /mute" (Just ("Muted mate",(ClientState {username="friend",quit=False,muted=Set.fromList ["mate"] }), (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))) (isCommand "/mute mate" (ClientState {username="friend",quit=False,muted=Set.empty }) (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))

--unmute command
testUnmute = TestCase $ assertEqual "For /unmute" (Just ("Unmuted mate",(ClientState {username="friend",quit=False,muted=Set.empty }), (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))) (isCommand "/unmute mate" (ClientState {username="friend",quit=False,muted=Set.fromList ["mate"] }) (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))

--users command
testUsers = TestCase $ assertEqual "For /users" (Just ("buddy, friend, mate",(ClientState {username="friend",quit=False,muted=Set.empty }), (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))) (isCommand "/users" (ClientState {username="friend",quit=False,muted=Set.empty }) (ServerState {users=Map.fromList [("mate","123"),("buddy","qwerty"),("friend","zxc")],onlineUsers=Set.fromList ["friend","mate","buddy"]}))



--run all tests
runtests = runTestTT $ TestList [testQuit,testMute,testUnmute,testUsers]