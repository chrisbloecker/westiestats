{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database
  where
--------------------------------------------------------------------------------
import           ClassyPrelude       hiding (empty)
import           Competitor                 (getPointsAsIn)
import           Control.Monad.State        (get, put)
import           Data.Acid                  (Update, Query, makeAcidic)
import           Data.IxSet          hiding ((&&&))
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           Data.SafeCopy              (deriveSafeCopy, base)
import           Data.Text                  (inits)
import           Model
--------------------------------------------------------------------------------
import qualified Data.IxSet          as Ix  (toList)
--------------------------------------------------------------------------------

data Database = Database { snapshotDate :: !Day
                         , competitors  :: !(IxSet Competitor)
                         , events       :: !(IxSet EventDetails)
                         }

instance Indexable Competitor where
  empty = ixSet [ ixFun $ \Competitor{..} -> [ competitorId    ]
                , ixFun $ \Competitor{..} -> [ competitorWscId ]
                , ixFun $ \Competitor{..} -> concatMap (map (eventId   . competitionEvent) . resultCompetitions) competitorResults
                , ixFun $ \Competitor{..} -> concatMap (map (eventYear . competitionEvent) . resultCompetitions) competitorResults
                , ixFun $ \Competitor{..} -> map resultDivision competitorResults
                , ixFun $ \Competitor{..} -> [ Prefix prefix | prefix <- nub . concatMap inits $ [ competitorFirstName
                                                                                                 , competitorLastName
                                                                                                 , unwords [competitorFirstName, competitorLastName]
                                                                                                 , pack . show . unWscId $ competitorWscId
                                                                                                 ]
                                                             , length prefix >= 3
                                             ]
                ]

instance Indexable EventDetails where
  empty = ixSet [ ixFun $ \EventDetails{..} -> [ eventDetailsId ]
                ]

--------------------------------------------------------------------------------
deriveSafeCopy 0 'base ''Database
deriveSafeCopy 0 'base ''Competitor
deriveSafeCopy 0 'base ''Result
deriveSafeCopy 0 'base ''Competition
deriveSafeCopy 0 'base ''Event
deriveSafeCopy 0 'base ''EventDetails
deriveSafeCopy 0 'base ''Division
deriveSafeCopy 0 'base ''Placement
deriveSafeCopy 0 'base ''Role
--------------------------------------------------------------------------------
deriveSafeCopy 0 'base ''CompetitorId
deriveSafeCopy 0 'base ''WscId
deriveSafeCopy 0 'base ''ResultPoints
deriveSafeCopy 0 'base ''EventId
deriveSafeCopy 0 'base ''EventYear
deriveSafeCopy 0 'base ''Prefix
--------------------------------------------------------------------------------

initDatabase :: Database
initDatabase = Database { snapshotDate = fromGregorian 1970 1 1
                        , competitors  = empty
                        , events       = empty
                        }

--------------------------------------------------------------------------------

setSnapshotDate :: Day -> Update Database ()
setSnapshotDate date = do
  db@Database{..} <- get
  put $ db { snapshotDate = date }


getSnapshotDate :: Query Database Day
getSnapshotDate = fmap snapshotDate ask


getCompetitor :: WscId -> Query Database (Maybe Competitor)
getCompetitor wscid = fmap (getOne . getEQ wscid . competitors) ask


insertCompetitor :: Competitor -> Update Database ()
insertCompetitor competitor@Competitor{..} = do
  db@Database{..} <- get
  put $ db { competitors = updateIx competitorId competitor competitors }


searchPrefix :: Prefix -> Query Database [Competitor]
searchPrefix prefix = fmap (Data.IxSet.toList . getEQ prefix . competitors) ask


getMostPoints :: Role -> Division -> Query Database [(Competitor, ResultPoints)]
getMostPoints role division = do
  cs <- fmap (Ix.toList . getEQ division . competitors) ask
  return . reverse . sortOn snd . fmap (id &&& fromJust . getPointsAsIn role division) $ cs


getEventDetails :: EventId -> Query Database (Maybe EventDetails)
getEventDetails eventId = fmap (getOne . getEQ eventId . events) ask


insertEventDetails :: EventDetails -> Update Database ()
insertEventDetails eventDetails@EventDetails{..} = do
  db@Database{..} <- get
  let moldDetails = getOne . getEQ eventDetailsId $ events
  case moldDetails of
    Nothing         -> put $ db { events = updateIx eventDetailsId  eventDetails                                   events }
    Just oldDetails -> put $ db { events = updateIx eventDetailsId (eventDetails `combineEventDetails` oldDetails) events }


getEvents :: Query Database [EventDetails]
getEvents = fmap (Ix.toList . events) ask

--------------------------------------------------------------------------------

makeAcidic ''Database [ 'setSnapshotDate
                      , 'getSnapshotDate

                      , 'getCompetitor
                      , 'insertCompetitor
                      , 'searchPrefix

                      , 'getMostPoints

                      , 'getEventDetails
                      , 'insertEventDetails

                      , 'getEvents
                      ]
