module GetNetRc.Format
    ( formatOutput
    ) where

import GetNetRc.Types

formatOutput :: Output -> String
formatOutput (Error msg) = "error: " ++ msg ++ "\n"
formatOutput (MachineValues [[Value _ x]]) = x ++ "\n"
formatOutput (MachineValues [vs]) = concatMap (showValue "") vs
formatOutput (MachineValues vvs) = concat $ concatMap showMany $ zip [1..] vvs

showMany :: (Int, [Value]) -> [String]
showMany (i, vs) = ("machine " ++ show i ++ "\n") : map (showValue "  ") vs

showValue :: String -> Value -> String
showValue pre (Value a b) = pre ++ show a ++ ": " ++ b ++ "\n"
