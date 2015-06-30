module Model.Division
  where

-------------------------------------------------------------------------------
import ClassyPrelude
import Import.DeriveJSON
import Model.Placement
-------------------------------------------------------------------------------

data Division = Division { name                :: Text
                         , leader_points       :: Int
                         , follower_points     :: Int
                         , leader_placements   :: [Placement]
                         , follower_placements :: [Placement]
                         }

$(deriveJSON jsonOptions ''Division)