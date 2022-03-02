{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import GHC.Generics


data User = User {
      username :: String
    , password :: String
    } deriving (Generic, Show)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User
    

writeUserToFile user = encodeFile "users.json" user
