{-# LANGUAGE RecordWildCards #-}
module GetNetRc
    ( Options(..)
    , Field(..)
    , Filter(..)
    , getNetRc
    ) where

import qualified Data.ByteString.Char8 as C8
import GetNetRc.Types
import Network.NetRc (NetRc(..), NetRcHost(..), parseNetRc)

getNetRc :: Options -> Output
getNetRc Options {..} =
    either (Error . show) (filterOutput oOutputFields . findHosts oFilters)
        $ parseNetRc oFilePath oFileContents

findHosts :: [Filter] -> NetRc -> [NetRcHost]
findHosts fs = filter (\h -> all (matchesFilter h) fs) . nrHosts

filterOutput :: [Field] -> [NetRcHost] -> Output
filterOutput fs = MachineValues . map valuesAtFields
    where valuesAtFields h = map (valueAtField h) fs

matchesFilter :: NetRcHost -> Filter -> Bool
matchesFilter h (Filter f v) = accessField f h == v

valueAtField :: NetRcHost -> Field -> Value
valueAtField h f = Value f $ accessField f h

accessField :: Field -> NetRcHost -> String
accessField Name = C8.unpack . nrhName
accessField Login = C8.unpack . nrhLogin
accessField Password = C8.unpack . nrhPassword
accessField Account = C8.unpack . nrhAccount
