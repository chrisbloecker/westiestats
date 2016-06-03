module Model.External
  where

--------------------------------------------------------------------------------
import           ClassyPrelude
import           Import.DeriveJSON
import           Text.Blaze         (ToMarkup (..))
--------------------------------------------------------------------------------

data Person = Person { personType       :: Text
                     , personDancer     :: Dancer
                     , personPlacements :: Maybe Placements
                     }
  deriving (Show)

data Dancer = Dancer { dancerId        :: Integer
                     , dancerFirstName :: Text
                     , dancerLastName  :: Text
                     , dancerWscid     :: Integer
                     }
  deriving (Show)

data Placements = Placements { westCoastSwing :: Maybe [Division]
                             , lindy          :: Maybe [Division]
                             }
  deriving (Show)

data Division = Division { divisionDetails      :: Details
                         , divisionTotalPoints  :: Integer
                         , divisionCompetitions :: [Competition]
                         }
  deriving (Show)

data Details = Details { detailsId           :: Integer
                       , detailsName         :: Text
                       , detailsAbbreviation :: Text
                       }
  deriving (Show)

data Competition = Competition { competitionRole   :: Text
                               , competitionPoints :: Integer
                               , competitionEvent  :: Event
                               , competitionResult :: Text
                               }
  deriving (Show)

data Event = Event { eventId       :: Integer
                   , eventName     :: Text
                   , eventLocation :: Text
                   , eventUrl      :: Maybe Text
                   , eventDate     :: Text
                   }
  deriving (Show)

instance ToMarkup Person where
  toMarkup = toMarkup . show

instance ToMarkup Dancer where
  toMarkup = toMarkup . show

instance ToMarkup Placements where
  toMarkup = toMarkup . show

instance ToMarkup Division where
  toMarkup = toMarkup . show

instance ToMarkup Details where
  toMarkup = toMarkup . show

instance ToMarkup Competition where
  toMarkup = toMarkup . show

instance ToMarkup Event where
  toMarkup = toMarkup . show

$(deriveJSON jsonOptions ''Person)
$(deriveJSON jsonOptions ''Dancer)
$(deriveJSON jsonOptions ''Placements)
$(deriveJSON jsonOptions ''Division)
$(deriveJSON jsonOptions ''Details)
$(deriveJSON jsonOptions ''Competition)
$(deriveJSON jsonOptions ''Event)
