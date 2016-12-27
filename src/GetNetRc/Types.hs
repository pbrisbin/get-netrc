module GetNetRc.Types
    ( Options(..)
    , Field(..)
    , Filter(..)
    , Value(..)
    , Output(..)
    ) where

import Data.ByteString (ByteString)

data Options = Options
    { oFilePath :: FilePath
    , oFileContents :: ByteString
    , oFilters :: [Filter]
    , oOutputFields :: [Field]
    }

data Field
    = Name
    | Login
    | Password
    | Account
    deriving (Bounded, Enum, Eq, Read, Show)

data Filter = Filter Field String deriving (Eq, Show)

data Value = Value Field String deriving (Eq, Show)

data Output = Error String | MachineValues [[Value]] deriving (Eq, Show)
