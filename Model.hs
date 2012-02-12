module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time (Day)
import Data.Aeson
import Control.Monad (mzero)

data Status = Open | Done | OnHold | Ignore
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ToJSON Status where
    toJSON Open = String "open"
    toJSON Done = String "done"
    toJSON OnHold = String "on_hold"
    toJSON Ignore = String "ignore"

instance FromJSON Status where
    parseJSON (String "open") = return Open
    parseJSON (String "done") = return Done
    parseJSON (String "on_hold") = return OnHold
    parseJSON (String "ignore") = return Ignore
    parseJSON _ = mzero

derivePersistField "Status"

-- | Can only have values 1 to 5
newtype Priority = Priority Int
    deriving (Show, Eq, Read, PersistField)

instance ToJSON Priority where
    toJSON (Priority i) = toJSON i

instance FromJSON Priority where
    parseJSON v = do
        i <- parseJSON v
        if 1 <= i && i <= 5
            then return $ Priority i
            else mzero

instance ToJSON Day where
    toJSON = toJSON . show

instance FromJSON Day where
    parseJSON v = do
        s <- parseJSON v
        case reads s of
            (d, _):_ -> return d
            [] -> mzero

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
