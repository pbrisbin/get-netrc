module Lib.GPG
    ( decryptFile
    ) where

import Data.ByteString (ByteString)
import System.Process (readProcess)

import qualified Data.ByteString.Char8 as C8

decryptFile :: [String] -> FilePath -> IO ByteString
decryptFile options input = gpg $ options ++
    [ "--no-mdc-warning"
    , "--quiet"
    , "--batch"
    , "--decrypt"
    , input
    ]

gpg :: [String] -> IO ByteString
gpg options = C8.pack <$> readProcess "gpg" options ""
