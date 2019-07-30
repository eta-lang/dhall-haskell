{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (optional)
import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Version (showVersion)
import Dhall.JSONToDhall (Conversion, parseConversion)
import Dhall.Pretty (CharacterSet(..))
import Dhall.YamlToDhall (Options(..), dhallFromYaml)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.ByteString.Char8                     as BSL8
import qualified Data.Text.IO                              as Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Dhall.Pretty
import qualified GHC.IO.Encoding
import qualified Options.Applicative                       as Options
import qualified System.Console.ANSI                       as ANSI
import qualified System.Exit
import qualified System.IO                                 as IO
import qualified Paths_dhall_json                          as Meta

-- ---------------
-- Command options
-- ---------------

data CommandOptions = CommandOptions
    { version    :: Bool
    , schema     :: Text
    , conversion :: Conversion
    , file       :: Maybe FilePath
    , ascii      :: Bool
    , plain      :: Bool
    } deriving Show

-- | Command info and description
parserInfo :: ParserInfo CommandOptions
parserInfo = Options.info
          (  Options.helper <*> parseOptions)
          (  Options.fullDesc
          <> Options.progDesc "Convert a YAML expression to a Dhall expression, given the expected Dhall type"
          )



-- | Parser for all the command arguments and options
parseOptions :: Parser CommandOptions
parseOptions = CommandOptions <$> parseVersion
                              <*> parseSchema
                              <*> parseConversion
                              <*> optional parseFile
                              <*> parseASCII
                              <*> parsePlain
  where
    parseSchema =
        Options.strArgument
            (  Options.metavar "SCHEMA"
            <> Options.help "Dhall type expression (schema)"
            )

    parseVersion =
        Options.switch
            (  Options.long "version"
            <> Options.short 'V'
            <> Options.help "Display version"
            )

    parseFile =
        Options.strOption
            (   Options.long "file"
            <>  Options.help "Read YAML expression from a file instead of standard input"
            <>  Options.metavar "FILE"
            )

    parseASCII =
        Options.switch
            (   Options.long "ascii"
            <>  Options.help "Format code using only ASCII syntax"
            )

    parsePlain =
        Options.switch
            (   Options.long "plain"
            <>  Options.help "Disable syntax highlighting"
            )

-- ----------
-- Main
-- ----------

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    CommandOptions{..} <- Options.execParser parserInfo

    let characterSet = case ascii of
            True  -> ASCII
            False -> Unicode

    when version $ do
      putStrLn (showVersion Meta.version)
      System.Exit.exitSuccess

    handle $ do
        bytes <- case file of
            Nothing   -> BSL8.getContents
            Just path -> BSL8.readFile path

        result <- dhallFromYaml (Options schema conversion) bytes

        let document = Dhall.Pretty.prettyCharacterSet characterSet result

        let stream = Pretty.layoutSmart Dhall.Pretty.layoutOpts document

        supportsANSI <- ANSI.hSupportsANSI IO.stdout

        let ansiStream =
                if supportsANSI && not plain
                then fmap Dhall.Pretty.annToAnsiStyle stream
                else Pretty.unAnnotateS stream

        Pretty.Terminal.renderIO IO.stdout ansiStream

        Text.IO.putStrLn ""


handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        IO.hPutStrLn IO.stderr ""
        IO.hPrint    IO.stderr e
        System.Exit.exitFailure
