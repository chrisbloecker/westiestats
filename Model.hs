module Model
  where

-------------------------------------------------------------------------------
import ClassyPrelude
import Data.IxSet
import Import.DeriveJSON
import Model.Person
-------------------------------------------------------------------------------

newtype Database = Database { persons :: [Person] }

$(deriveJSON jsonOptions ''Database)

instance Indexable Person where
  empty = ixSet [ ixFun $ \p -> [wscid p] ]

-------------------------------------------------------------------------------

--initDatabase :: Database
--initDatabase = Database { persons = empty }