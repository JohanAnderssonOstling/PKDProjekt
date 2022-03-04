module Types where
import Data.Map (Map)

import qualified Data.Map as Map
type Username = String
type Password = String
type UserMap = Map.Map Username Password
