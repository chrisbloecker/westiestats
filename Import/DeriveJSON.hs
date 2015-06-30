module Import.DeriveJSON
  ( FromJSON, decode'
  , deriveJSON, jsonOptions
  , Text
  , encodeUtf8
  ) where

-------------------------------------------------------------------------------
import ClassyPrelude
import Data.Aeson    (FromJSON, decode')
import Data.Aeson.TH (Options (..), SumEncoding (..), deriveJSON)
-------------------------------------------------------------------------------

jsonOptions :: Options
jsonOptions = Options { fieldLabelModifier      = id
                      , constructorTagModifier  = id
                      , allNullaryToStringTag   = True
                      , omitNothingFields       = False
                      , sumEncoding             = ObjectWithSingleField
                      }