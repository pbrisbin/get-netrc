module Main where

import GetNetRc
import GetNetRc.Format
import GetNetRc.Options

main :: IO ()
main = putStr . formatOutput . getNetRc =<< parseOptions
