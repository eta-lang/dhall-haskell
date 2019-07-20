{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Yaml
  ( Options(..)
  , parseDocuments
  , parseQuoted
  , defaultOptions
  , dhallToYaml ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.JSON (Conversion(..), SpecialDoubleMode(..), codeToValue)
import Options.Applicative (Parser)
import Data.ByteString.Lazy (toStrict)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Vector
import qualified Dhall
import qualified Options.Applicative
#if defined(ETA_VERSION)
import Dhall.Yaml.Eta ( jsonToYaml )
#else
import qualified Data.YAML.Aeson as YAML
import qualified Data.YAML as Y
import qualified Data.YAML.Event as YE
import qualified Data.YAML.Token as YT
import qualified Data.Text
#endif


data Options = Options
    { explain    :: Bool
    , omission   :: Data.Aeson.Value -> Data.Aeson.Value
    , documents  :: Bool
    , quoted     :: Bool
    , conversion :: Conversion
    }

defaultOptions :: Options
defaultOptions =
  Options { explain = False
          , omission = id
          , documents = False
          , quoted = False
          , conversion = NoConversion
          }

parseDocuments :: Parser Bool
parseDocuments =
  Options.Applicative.switch
            (   Options.Applicative.long "documents"
            <>  Options.Applicative.help "If given a Dhall list, output a document for every element"
            )

parseQuoted :: Parser Bool
parseQuoted =
  Options.Applicative.switch
            (   Options.Applicative.long "quoted"
            <>  Options.Applicative.help "Prevent from generating not quoted scalars"
            )
                           
{-| Convert a piece of Text carrying a Dhall inscription to an equivalent YAML ByteString
-}
dhallToYaml
  :: Options
  -> Text  -- ^ Describe the input for the sake of error location.
  -> Text  -- ^ Input text.
  -> IO ByteString
dhallToYaml Options{..} name code = do
  
  let explaining = if explain then Dhall.detailed else id

  json <- omission <$> explaining (codeToValue conversion UseYAMLEncoding name code)

  return $ jsonToYaml json documents quoted


{-# INLINE bsToStrict #-}
bsToStrict :: Data.ByteString.Lazy.ByteString -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
bsToStrict = Data.ByteString.Lazy.toStrict
#else
bsToStrict = Data.ByteString. Data.ByteString.Lazy.toChunks
#endif

#if !defined(ETA_VERSION)
-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =

  case (documents, json) of
    (True, Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap (bsToStrict. (YAML.encodeValue' schemaEncoder YT.UTF8). (:[]))
         $ Data.Vector.toList elems
    _ -> bsToStrict (YAML.encodeValue' schemaEncoder YT.UTF8 [json])
  where
    defaultSchemaEncoder = Y.coreSchemaEncoder
    
    customStyle (Y.SStr s) = case () of
        ()
            | "\n" `Data.Text.isInfixOf` s -> Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
            | otherwise -> Right (YE.untagged, YE.SingleQuoted, s)
    customStyle scalar =  (Y.schemaEncoderScalar defaultSchemaEncoder) scalar
    
    setScalarStyle sty encoder = encoder {Y.schemaEncoderScalar = sty}
    
    customSchemaEncoder = setScalarStyle 
                            customStyle
                            defaultSchemaEncoder
    
    schemaEncoder = if quoted 
        then customSchemaEncoder 
        else defaultSchemaEncoder
#endif
