module Lib.Types
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
    deriving (Enum, Bounded, Read, Show)

data Filter = Filter Field String deriving Show
data Value = Value Field String deriving Show
data Output = Error String | MachineValues [[Value]] deriving Show
