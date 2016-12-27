module Main where

import Lib
import Lib.Format
import Lib.Options

main :: IO ()
main = putStr . formatOutput . getNetRc =<< parseOptions
