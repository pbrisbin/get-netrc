{-# LANGUAGE RecordWildCards #-}
module Lib.Options
    ( parseOptions
    ) where

import Lib.Types
import Lib.GPG

import Data.List (intercalate)
import Options.Applicative
import System.FilePath ((</>), (<.>))
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS

data Opts = Opts
    { oOutputFields' :: [Field]
    , oFilters' :: [Filter]
    , oFilePath' :: Maybe FilePath
    }

toOptions :: Opts -> IO Options
toOptions Opts{..} = do
    path <- maybe (pathInHome ".netrc") return oFilePath'
    let gpgPath = path <.> "gpg"
    gpgExists <- doesFileExist gpgPath
    contents <- if gpgExists
        then decryptFile [] gpgPath
        else BS.readFile path

    return Options
        { oFilePath = path
        , oFileContents = contents
        , oFilters = oFilters'
        , oOutputFields = oOutputFields'
        }

parseOptions :: IO Options
parseOptions = toOptions =<< execParser
    (info (helper <*> parser)
        $  fullDesc
        <> progDesc "Read credentials out of ~/.netrc"
        <> footer ("Available fields: " ++ intercalate ", " allFields)
        )

  where
    allFields = map show ([minBound..maxBound] :: [Field])

parser :: Parser Opts
parser = Opts
    <$> many (option auto
        (  short 'o'
        <> long "output"
        <> metavar "Field"
        <> help "Fields to output"
        ))
    <*> many (option (eitherReader parseFilter)
        (  short 'f'
        <> long "filter"
        <> metavar "Field=value"
        <> help "Filter the machines returned"
        ))
    <*> optional (argument auto
        (  metavar "PATH"
        <> help "Alternate path to ~/.netrc"
        ))

parseFilter :: String -> Either String Filter
parseFilter x =
    let err = Left $ "Invalid filter " ++ x
        fld = takeWhile (/= '=') x
        val = drop 1 $ dropWhile (/= '=') x
    in maybe err (\n -> Right $ Filter n val) $ readMaybe fld

pathInHome :: FilePath -> IO FilePath
pathInHome p = do
    h <- getEnv "HOME"
    return $ h </> p
