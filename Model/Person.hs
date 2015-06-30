module Model.Person
  where

-------------------------------------------------------------------------------
import ClassyPrelude
import Import.DeriveJSON
import Model.Result
-------------------------------------------------------------------------------

data Person = Person { full_name :: Text
                     , wscid     :: Int
                     , results   :: [Result]
                     }

$(deriveJSON jsonOptions ''Person)