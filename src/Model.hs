module Model
  where

--------------------------------------------------------------------------------
import           ClassyPrelude
import           Data.List           ((!!))
import           Text.Blaze          (ToMarkup (..))
import           Text.Read           (read)
import           Web.PathPieces      (PathPiece (..))
--------------------------------------------------------------------------------
import qualified Data.Set       as S (union)
import qualified Data.Map       as M (unionWith)
import qualified Model.External as E
--------------------------------------------------------------------------------

data Competitor = Competitor { competitorId      :: CompetitorId
                             , competitorWscId   :: WscId
                             , competitorName    :: Text
                             , competitorResults :: [Result]
                             }
  deriving (Eq, Ord)

data Result = Result { resultDivision     :: Division
                     , resultPoints       :: Integer
                     , resultCompetitions :: [Competition]
                     }
  deriving (Eq, Ord)

data Competition = Competition { competitionEvent     :: Event
                               , competitionRole      :: Role
                               , competitionPlacement :: Placement
                               , competitionPoints    :: Integer
                               }
  deriving (Eq, Ord)

data Event = Event { eventId       :: EventId
                   , eventName     :: Text
                   , eventLocation :: Text
                   , eventMonth    :: Text
                   , eventYear     :: EventYear
                   }
  deriving (Eq, Ord)

data EventDetails = EventDetails { eventDetailsId       :: EventId
                                 , eventDetailsName     :: Text
                                 , eventDetailsLocation :: Text
                                 , eventDetailsResults  :: Map EventYear (Map Division (Set ResultEntry))
                                 }
  deriving (Eq, Ord, Show)

type ResultEntry = (WscId, Text, Role, Placement)

data Division = Newcomer
              | Novice
              | Intermediate
              | Advanced
              | Allstars
              | Champions
              | Invitational
              | Professional
              | Juniors
              | Sophisticated
              | Masters
              | Teacher
  deriving (Eq, Ord, Show)

data Placement = One
               | Two
               | Three
               | Four
               | Five
               | Finals
  deriving (Eq, Ord, Show)

data Role = Leader
          | Follower
  deriving (Eq, Ord, Show)

newtype CompetitorId = CompetitorId { unCompetitorId :: Integer } deriving (Eq, Ord)
newtype WscId        = WscId        { unWscId        :: Integer } deriving (Eq, Ord, Show, Read, PathPiece)
newtype EventId      = EventId      { unEventId      :: Integer } deriving (Eq, Ord, Show, Read, PathPiece)
newtype EventYear    = EventYear    { unEventYear    :: Integer } deriving (Eq, Ord, Show, Read, PathPiece)
--------------------------------------------------------------------------------

instance ToMarkup Division where
  toMarkup = toMarkup . show

instance ToMarkup Placement where
  toMarkup One    = "1"
  toMarkup Two    = "2"
  toMarkup Three  = "3"
  toMarkup Four   = "4"
  toMarkup Five   = "5"
  toMarkup Finals = "F"

instance ToMarkup Role where
  toMarkup = toMarkup . show

instance ToMarkup CompetitorId where
  toMarkup = toMarkup . unCompetitorId

instance ToMarkup WscId where
  toMarkup = toMarkup . unWscId

instance ToMarkup EventYear where
  toMarkup = toMarkup . unEventYear

instance Monoid EventDetails where
  ed1 `mappend` ed2 = EventDetails { eventDetailsId       = eventDetailsId       ed1
                                   , eventDetailsName     = eventDetailsName     ed1
                                   , eventDetailsLocation = eventDetailsLocation ed1
                                   , eventDetailsResults  = M.unionWith (M.unionWith S.union) (eventDetailsResults ed1) (eventDetailsResults ed2)
                                   }

--------------------------------------------------------------------------------

toDivision :: Text -> Division
toDivision "Newcomer"      = Newcomer
toDivision "Novice"        = Novice
toDivision "Intermediate"  = Intermediate
toDivision "Advanced"      = Advanced
toDivision "All-Stars"     = Allstars
toDivision "Champions"     = Champions
toDivision "Invitational"  = Invitational
toDivision "Professional"  = Professional
toDivision "Juniors"       = Juniors
toDivision "Sophisticated" = Sophisticated
toDivision "Masters"       = Masters
toDivision "Teacher"       = Teacher
toDivision d               = error ("Unknown division " ++ unpack d)

toPlacement :: Text -> Placement
toPlacement "1" = One
toPlacement "2" = Two
toPlacement "3" = Three
toPlacement "4" = Four
toPlacement "5" = Five
toPlacement "F" = Finals
toPlacement p   = error ("Unknown placement " ++ unpack p)

toRole :: Text -> Role
toRole "leader"   = Leader
toRole "follower" = Follower
toRole r          = error ("Unknown role" ++ unpack r)

fromPerson :: E.Person -> Competitor
fromPerson E.Person{..} =
  Competitor { competitorId      = CompetitorId $ E.dancerId    personDancer
             , competitorWscId   = WscId        $ E.dancerWscid personDancer
             , competitorName    = unwords [E.dancerFirstName personDancer, E.dancerLastName personDancer]
             , competitorResults = fromPlacements personPlacements
             }

fromPlacements :: Maybe E.Placements -> [Result]
fromPlacements Nothing               = []
fromPlacements (Just E.Placements{..}) =
  case westCoastSwing of
    Nothing -> []
    Just ds -> fmap fromDivision ds

fromDivision :: E.Division -> Result
fromDivision E.Division{..} =
  Result { resultDivision     = toDivision (E.detailsName divisionDetails)
         , resultPoints       = divisionTotalPoints
         , resultCompetitions = fmap fromCompetition divisionCompetitions
         }

fromCompetition :: E.Competition -> Competition
fromCompetition E.Competition{..} =
  Competition { competitionEvent     = fromEvent competitionEvent
              , competitionRole      = toRole competitionRole
              , competitionPlacement = toPlacement competitionResult
              , competitionPoints    = competitionPoints
              }

fromEvent :: E.Event -> Event
fromEvent E.Event{..} =
  Event { eventId       = EventId eventId
        , eventName     = eventName
        , eventLocation = eventLocation
        , eventMonth    =                       words eventDate !! 0
        , eventYear     = EventYear. readInt $ (words eventDate !! 1)
        }

readInt :: Text -> Integer
readInt = (read :: String -> Integer) . unpack
