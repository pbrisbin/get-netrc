{-# LANGUAGE RecordWildCards #-}
module Lib
    ( Options(..)
    , Field(..)
    , Filter(..)
    , getNetRc
    ) where

import Lib.Types
import Network.NetRc (NetRc(..), NetRcHost(..), parseNetRc)

import qualified Data.ByteString.Char8 as C8

getNetRc :: Options -> Output
getNetRc Options{..} = either
    (Error . show)
    (filterOutput oOutputFields . findHosts oFilters)
    $ parseNetRc oFilePath oFileContents

findHosts :: [Filter] -> NetRc -> [NetRcHost]
findHosts fs = filter (\h -> all (matchesField h) fs) . nrHosts
  where
    matchesField NetRcHost{..} (Filter Name n) = nrhName `bsEq` n
    matchesField NetRcHost{..} (Filter Login l) = nrhLogin `bsEq` l
    matchesField NetRcHost{..} (Filter Password p) = nrhPassword `bsEq` p
    matchesField NetRcHost{..} (Filter Account a) = nrhAccount `bsEq` a

    bsEq bs = (== bs) . C8.pack

filterOutput :: [Field] -> [NetRcHost] -> Output
filterOutput fs = MachineValues . map getFields
    where
        getFields n = map (getField n) fs

        getField NetRcHost{..} Name = Value Name $ C8.unpack nrhName
        getField NetRcHost{..} Login = Value Login $ C8.unpack nrhLogin
        getField NetRcHost{..} Password = Value Password $ C8.unpack nrhPassword
        getField NetRcHost{..} Account = Value Account $ C8.unpack nrhAccount
