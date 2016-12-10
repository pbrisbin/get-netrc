module Main where

import Lib
import Lib.Format
import Lib.Options

main :: IO ()
main = do
    opts <- parseOptions
    putStr $ formatOutput $ getNetRc opts
