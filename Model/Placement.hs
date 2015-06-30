module Model.Placement
  where

-------------------------------------------------------------------------------
import ClassyPrelude
import Import.DeriveJSON
-------------------------------------------------------------------------------

data Placement = Placement { result     :: Text
                           , points     :: Text
                           , end_date   :: Text
                           , start_date :: Text
                           , location   :: Text
                           , name       :: Text
                           }

$(deriveJSON jsonOptions ''Placement)