{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
--------------------------------------------------------------------------------
module Model
  ( module Model
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude
import Data.List           ((!!))
import Import.DeriveJSON
import Model.Location as Model
import Text.Blaze          (ToMarkup (..))
import Text.Read           (read)
import Web.PathPieces      (PathPiece (..))
--------------------------------------------------------------------------------
import qualified Data.Set       as S (union)
import qualified Data.Map       as M (unionWith)
import qualified Model.External as E
--------------------------------------------------------------------------------

data Competitor = Competitor { competitorId        :: !CompetitorId
                             , competitorWscId     :: !WscId
                             , competitorFirstName :: !Text
                             , competitorLastName  :: !Text
                             , competitorResults   :: ![Result]
                             }
  deriving (Eq, Ord, Show)

data Result = Result { resultDivision     :: !Division
                     , resultPoints       :: !ResultPoints
                     , resultCompetitions :: ![Competition]
                     }
  deriving (Eq, Ord, Show)

data Competition = Competition { competitionEvent     :: !Event
                               , competitionRole      :: !Role
                               , competitionPlacement :: !Placement
                               , competitionPoints    :: !Integer
                               }
  deriving (Eq, Ord, Show)

data Event = Event { eventId       :: !EventId
                   , eventName     :: !Text
                   , eventLocation :: !Location
                   , eventMonth    :: !Text
                   , eventYear     :: !EventYear
                   }
  deriving (Eq, Ord, Show)

data EventDetails = EventDetails { eventDetailsId       :: !EventId
                                 , eventDetailsName     :: !Text
                                 , eventDetailsLocation :: !Location
                                 , eventDetailsResults  :: !(Map EventYear (Map Division (Set ResultEntry)))
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
  deriving (Eq, Ord, Enum, Show)

preferredDivisionOrder :: [Division]
preferredDivisionOrder = [ Invitational, Champions, Allstars, Advanced, Intermediate, Novice, Newcomer
                         , Masters, Sophisticated
                         , Professional, Teacher, Juniors
                         ]

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

newtype CompetitorId = CompetitorId { unCompetitorId :: Integer } deriving (Eq, Ord, Show)
newtype WscId        = WscId        { unWscId        :: Integer } deriving (Eq, Ord, Show, Read, PathPiece)
newtype ResultPoints = ResultPoints { unResultPoints :: Integer } deriving (Eq, Ord, Show)
newtype EventId      = EventId      { unEventId      :: Integer } deriving (Eq, Ord, Show, Read, PathPiece)
newtype EventYear    = EventYear    { unEventYear    :: Integer } deriving (Eq, Ord, Show, Read, PathPiece)
newtype Prefix       = Prefix       { unPrefix       :: Text    } deriving (Eq, Ord, Show)
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

instance ToMarkup ResultPoints where
  toMarkup = toMarkup . unResultPoints

instance ToMarkup EventYear where
  toMarkup = toMarkup . unEventYear

instance ToMarkup Day where
  toMarkup = toMarkup . show

--------------------------------------------------------------------------------

newtype Suggestions = Suggestions { unSuggestions :: [Suggestion] }

data Suggestion = Suggestion { suggestionValue :: Text
                             , suggestionData  :: Integer
                             }
  deriving (Show)

mkSuggestion :: Competitor -> Suggestion
mkSuggestion Competitor{..} = let value = unwords [competitorFirstName, competitorLastName, "(" ++ (pack . show . unWscId $ competitorWscId) ++ ")"]
                                  data' = unWscId competitorWscId
                              in Suggestion value data'

$(deriveJSON jsonOptions ''Suggestions)
$(deriveJSON jsonOptions ''Suggestion)

--------------------------------------------------------------------------------

combineEventDetails :: EventDetails -> EventDetails -> EventDetails
combineEventDetails ed1 ed2 = EventDetails { eventDetailsId       = eventDetailsId       ed1
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
  Competitor { competitorId        = CompetitorId $ E.dancerId    personDancer
             , competitorWscId     = WscId        $ E.dancerWscid personDancer
             , competitorFirstName = E.dancerFirstName personDancer
             , competitorLastName  = E.dancerLastName personDancer
             , competitorResults   = fromPlacements personPlacements
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
         , resultPoints       = ResultPoints divisionTotalPoints
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
        , eventLocation = parseLocation eventLocation
        , eventMonth    =                       words eventDate !! 0
        , eventYear     = EventYear. readInt $ (words eventDate !! 1)
        }

readInt :: Text -> Integer
readInt = (read :: String -> Integer) . unpack
