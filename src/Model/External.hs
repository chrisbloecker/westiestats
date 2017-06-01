{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
--------------------------------------------------------------------------------
module Model.External
  where
--------------------------------------------------------------------------------
import ClassyPrelude
import Data.JsonStream.Parser ((.:), (.:?), integer, string, arrayOf)
import Data.Maybe             (fromJust)
import Data.Text              (splitOn, strip)
import Import.DeriveJSON
import Text.Blaze             (ToMarkup (..))
--------------------------------------------------------------------------------
import qualified Data.JsonStream.Parser as Stream
--------------------------------------------------------------------------------

data Snapshot = Snapshot { snapshotDate      :: !Day
                         , snapshotFromWscId :: !Integer
                         , snapshotToWscId   :: !Integer
                         , snapshotPersons   :: [Person]
                         }
  deriving (Show)

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
                   , eventLocation :: !(Maybe Location)
                   , eventUrl      :: !(Maybe Text)
                   , eventDate     :: !Text
                   }
  deriving (Show)

data Location = Location { locationCity    :: !Text
                         , locationCountry :: !Country
                         }
  deriving (Show)

data Country = Australia
             | Germany
             | Sweden
             | Finland
             | Norway
             | Unknown
  deriving (Show)

--------------------------------------------------------------------------------
-- Parsers for JsonStream

snapshot :: Stream.Parser Snapshot
snapshot = Snapshot <$>       getLabel "snapshotDate"      .: day
                    <*>       getLabel "snapshotFromWscId" .: parseInteger
                    <*>       getLabel "snapshottoWscId"   .: parseInteger
                    <*> many (getLabel "snapshotPersons"   .: arrayOf person)

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
event = Event <$>                  getLabel "eventId"       .:  parseInteger
              <*>                  getLabel "eventName"     .:  string
              <*> fmap toLocation (getLabel "eventLocation" .:  string)
              <*>                  getLabel "eventUrl"      .:? string
              <*>                  getLabel "eventDate"     .:  string

toLocation :: Text -> Maybe Location
toLocation t = case map strip . splitOn "," $ t of
  [""]            -> Nothing
  [      country] -> Just $ Location ""   (mkCountry country)
  [city, country] -> Just $ Location city (mkCountry country)
  _               -> fail $ "Unexpected form0at for location: " ++ unpack t

mkCountry :: Text -> Country
mkCountry "Australia" = Australia
mkCountry "Finland"   = Finland
mkCountry "Germany"   = Germany
mkCountry "Sweden"    = Sweden
mkCountry "Norway"    = Norway
mkCountry _           = Unknown

day :: Stream.Parser Day
day = fromJust . parseTimeM False defaultTimeLocale "%0Y-%m-%d" . unpack <$> string

parseInteger :: Stream.Parser Integer
parseInteger = (fromIntegral :: Int -> Integer) <$> integer

getLabel :: Text -> Text
getLabel = pack . fieldLabel . unpack

--------------------------------------------------------------------------------

instance ToMarkup Snapshot where
  toMarkup = toMarkup . show

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

$(deriveJSON jsonOptions ''Snapshot)
$(deriveJSON jsonOptions ''Person)
$(deriveJSON jsonOptions ''Dancer)
$(deriveJSON jsonOptions ''Placements)
$(deriveJSON jsonOptions ''Division)
$(deriveJSON jsonOptions ''Details)
$(deriveJSON jsonOptions ''Competition)
$(deriveJSON jsonOptions ''Event)
$(deriveJSON jsonOptions ''Location)
$(deriveJSON jsonOptions ''Country)
