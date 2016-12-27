{-# LANGUAGE OverloadedStrings #-}
module GetNetRcSpec
    ( main
    , spec
    ) where

import GetNetRc
import GetNetRc.Types
import Test.Hspec

import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "get-netrc" $ do
    it "parses an empty file correctly" $ do
        let options = Options
                { oFilePath = "test"
                , oFileContents = ""
                , oFilters = []
                , oOutputFields = []
                }

        getNetRc options `shouldBe` MachineValues []

    it "parses a single password by machine" $ do
        let options = Options
                { oFilePath = "test"
                , oFileContents = C8.unlines
                    [ "machine gmail.com"
                    , "  login pbrisbin@gmail.com"
                    , "  password secret"
                    , "machine gmx.com"
                    , "  login pbrisbin@gmx.com"
                    , "  password othersecret"
                    ]
                , oFilters = [Filter Name "gmail.com"]
                , oOutputFields = [Password]
                }

        getNetRc options `shouldBe` MachineValues [[Value Password "secret"]]

    it "parses multiple machines for multiple fields" $ do
        let options = Options
                { oFilePath = "test"
                , oFileContents = C8.unlines
                    [ "machine gmail.com"
                    , "  login pbrisbin@gmail.com"
                    , "  password secret"
                    , "machine gmx.com"
                    , "  login pbrisbin@gmx.com"
                    , "  password othersecret"
                    , "machine example.com"
                    , "  login pbrisbin@example.com"
                    , "  password otherothersecret"
                    ]
                , oFilters = []
                , oOutputFields = [Name, Login]
                }

        getNetRc options `shouldBe` MachineValues
            [ [Value Name "gmail.com", Value Login "pbrisbin@gmail.com"]
            , [Value Name "gmx.com", Value Login "pbrisbin@gmx.com"]
            , [Value Name "example.com", Value Login "pbrisbin@example.com"]
            ]

    it "returns parse errors" $ do
        let options = Options
                { oFilePath = "test"
                , oFileContents = C8.unlines
                    [ "floppity"
                    , "gibberish foo"
                    , "bar"
                    ]
                , oFilters = []
                , oOutputFields = []
                }

            isError (Error _) = True
            isError _ = False

        getNetRc options `shouldSatisfy` isError
