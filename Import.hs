module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Data.Aeson            as Import
import Data.Aeson.TH         as Import

jsonOptions :: Options
jsonOptions = Options { fieldLabelModifier      = id
                      , constructorTagModifier  = id
                      , allNullaryToStringTag   = True
                      , omitNothingFields       = False
                      , sumEncoding             = ObjectWithSingleField
                      }