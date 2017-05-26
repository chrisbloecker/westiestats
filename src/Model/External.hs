{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
--------------------------------------------------------------------------------
module Model.External
  where
--------------------------------------------------------------------------------
import ClassyPrelude
import Data.JsonStream.Parser ((.:), (.:?), integer, string, arrayOf)
import Import.DeriveJSON
import Text.Blaze             (ToMarkup (..))
--------------------------------------------------------------------------------
import qualified Data.JsonStream.Parser as Stream
--------------------------------------------------------------------------------

data Person = Person { personType       :: !Text
                     , personDancer     :: !Dancer
                     , personPlacements :: !(Maybe Placements)
                     }
  deriving (Show)

data Dancer = Dancer { dancerId        :: !Integer
                     , dancerFirstName :: !Text
                     , dancerLastName  :: !Text
                     , dancerWscid     :: !Integer
                     }
  deriving (Show)

data Placements = Placements { westCoastSwing :: !(Maybe [Division])
                             , lindy          :: !(Maybe [Division])
                             }
  deriving (Show)


data Division = Division { divisionDetails      :: !Details
                         , divisionTotalPoints  :: !Integer
                         , divisionCompetitions :: ![Competition]
                         }
  deriving (Show)

data Details = Details { detailsId           :: !Integer
                       , detailsName         :: !Text
                       , detailsAbbreviation :: !Text
                       }
  deriving (Show)

data Competition = Competition { competitionRole   :: !Text
                               , competitionPoints :: !Integer
                               , competitionEvent  :: !Event
                               , competitionResult :: !Text
                               }
  deriving (Show)

data Event = Event { eventId       :: !Integer
                   , eventName     :: !Text
                   , eventLocation :: !Text
                   , eventUrl      :: !(Maybe Text)
                   , eventDate     :: !Text
                   }
  deriving (Show)

--------------------------------------------------------------------------------
-- Parsers for JsonStream

person :: Stream.Parser Person
person = Person <$> getLabel "personType"       .:  string
                <*> getLabel "personDancer"     .:  dancer
                <*> getLabel "personPlacements" .:? placements

dancer :: Stream.Parser Dancer
dancer = Dancer <$> getLabel "dancerId"        .: parseInteger
                <*> getLabel "dancerFirstName" .: string
                <*> getLabel "dancerLastName"  .: string
                <*> getLabel "dancerWscid"     .: parseInteger

placements :: Stream.Parser Placements
placements = Placements <$> optional (many (getLabel "westCoastSwing" .: arrayOf division)) -- ToDo: can we write this in terms of .:? ?
                        <*> optional (many (getLabel "lindy"          .: arrayOf division))

division :: Stream.Parser Division
division = Division <$>       getLabel "divisionDetails"      .: details
                    <*>       getLabel "divisionTotalPoints"  .: parseInteger
                    <*> many (getLabel "divisionCompetitions" .: arrayOf competition)

details :: Stream.Parser Details
details = Details <$> getLabel "detailsId"           .: parseInteger
                  <*> getLabel "detailsName"         .: string
                  <*> getLabel "detailsAbbreviation" .: string

competition :: Stream.Parser Competition
competition = Competition <$> getLabel "competitionRole"   .: string
                          <*> getLabel "competitionPoints" .: parseInteger
                          <*> getLabel "competitionEvent"  .: event
                          <*> getLabel "competitionResult" .: string

event :: Stream.Parser Event
event = Event <$> getLabel "eventId"       .:  parseInteger
              <*> getLabel "eventName"     .:  string
              <*> getLabel "eventLocation" .:  string
              <*> getLabel "eventUrl"      .:? string
              <*> getLabel "eventDate"     .:  string

parseInteger :: Stream.Parser Integer
parseInteger = (fromIntegral :: Int -> Integer) <$> integer

getLabel :: Text -> Text
getLabel = pack . fieldLabel . unpack

--------------------------------------------------------------------------------

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
