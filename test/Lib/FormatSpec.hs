module Lib.FormatSpec
    ( main
    , spec
    ) where

import Lib.Format
import Lib.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "formatOutput" $ do
    it "formats errors" $ do
        let output = formatOutput $ Error "something happened"

        output `shouldBe` "error: something happened\n"

    it "formats a single output value" $ do
        let output = formatOutput $ MachineValues [[Value Password "secret"]]

        output `shouldBe` "secret\n"

    it "formats a single machine with multiple outputs" $ do
        let output = formatOutput $ MachineValues
                [ [ Value Login "user"
                  , Value Password "secret"
                  ]
                ]

        output `shouldBe` "Login: user\nPassword: secret\n"

    it "formats multiple machines" $ do
        let output = formatOutput $ MachineValues
                [ [ Value Login "user1"
                  , Value Password "secret1"
                  ]
                , [ Value Login "user2"
                  , Value Password "secret2"
                  ]
                ]

        output `shouldBe` "machine 1\n  Login: user1\n  Password: secret1\nmachine 2\n  Login: user2\n  Password: secret2\n"
