module Lib.Format
    ( formatOutput
    ) where

import Lib.Types

formatOutput :: Output -> String
formatOutput (Error msg) = "error: " ++ msg
formatOutput (MachineValues [[Value _ x]]) = x
formatOutput (MachineValues [vs]) = unlines $ map (showValue "") vs
formatOutput (MachineValues vvs) = unlines $ concatMap showMany $ zip [1..] vvs

showMany :: (Int, [Value]) -> [String]
showMany (i, vs) = ("machine " ++ show i) : map (showValue "  ") vs

showValue :: String -> Value -> String
showValue pre (Value a b) = pre ++ show a ++ ": " ++ b
