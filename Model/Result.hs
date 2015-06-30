module Model.Result
  where

-------------------------------------------------------------------------------
import ClassyPrelude
import Import.DeriveJSON
import Model.Division
-------------------------------------------------------------------------------

data Result = Result { name      :: Text
                     , divisions :: [Division]
                     }

$(deriveJSON jsonOptions ''Result)