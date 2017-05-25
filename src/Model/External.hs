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

data Person = Person { personType       :: Text
                     , personDancer     :: Dancer
                     , personPlacements :: Maybe Placements
                     }
  deriving (Show)

parsePerson :: Stream.Parser Person
parsePerson = Person <$> getLabel "personType"       .:  string
                     <*> getLabel "personDancer"     .:  parseDancer
                     <*> getLabel "personPlacements" .:? parsePlacements

data Dancer = Dancer { dancerId        :: Integer
                     , dancerFirstName :: Text
                     , dancerLastName  :: Text
                     , dancerWscid     :: Integer
                     }
  deriving (Show)

parseDancer :: Stream.Parser Dancer
parseDancer = Dancer <$> getLabel "dancerId"        .: parseInteger
                     <*> getLabel "dancerFirstName" .: string
                     <*> getLabel "dancerLastName"  .: string
                     <*> getLabel "dancerWscid"     .: parseInteger

data Placements = Placements { westCoastSwing :: Maybe [Division]
                             , lindy          :: Maybe [Division]
                             }
  deriving (Show)

parsePlacements :: Stream.Parser Placements
parsePlacements = Placements <$> optional (many (getLabel "westCoastSwing" .: arrayOf parseDivision)) -- ToDo: can we write this in terms of .:? ?
                             <*> optional (many (getLabel "lindy"          .: arrayOf parseDivision))

data Division = Division { divisionDetails      :: Details
                         , divisionTotalPoints  :: Integer
                         , divisionCompetitions :: [Competition]
                         }
  deriving (Show)

parseDivision :: Stream.Parser Division
parseDivision = Division <$>       getLabel "divisionDetails"      .: parseDetails
                         <*>       getLabel "divisionTotalPoints"  .: parseInteger
                         <*> many (getLabel "divisionCompetitions" .: arrayOf parseCompetition)

data Details = Details { detailsId           :: Integer
                       , detailsName         :: Text
                       , detailsAbbreviation :: Text
                       }
  deriving (Show)

parseDetails :: Stream.Parser Details
parseDetails = Details <$> getLabel "detailsId"           .: parseInteger
                       <*> getLabel "detailsName"         .: string
                       <*> getLabel "detailsAbbreviation" .: string

data Competition = Competition { competitionRole   :: Text
                               , competitionPoints :: Integer
                               , competitionEvent  :: Event
                               , competitionResult :: Text
                               }
  deriving (Show)

parseCompetition :: Stream.Parser Competition
parseCompetition = Competition <$> getLabel "competitionRole"   .: string
                               <*> getLabel "competitionPoints" .: parseInteger
                               <*> getLabel "competitionEvent"  .: parseEvent
                               <*> getLabel "competitionResult" .: string

data Event = Event { eventId       :: Integer
                   , eventName     :: Text
                   , eventLocation :: Text
                   , eventUrl      :: Maybe Text
                   , eventDate     :: Text
                   }
  deriving (Show)

parseEvent :: Stream.Parser Event
parseEvent = Event <$> getLabel "eventId"       .:  parseInteger
                   <*> getLabel "eventName"     .:  string
                   <*> getLabel "eventLocation" .:  string
                   <*> getLabel "eventUrl"      .:? string
                   <*> getLabel "eventDate"     .:  string

parseInteger :: Stream.Parser Integer
parseInteger = (fromIntegral :: Int -> Integer) <$> integer

getLabel :: Text -> Text
getLabel = pack . fieldLabel . unpack

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
