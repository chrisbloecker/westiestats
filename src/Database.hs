{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database
  where
--------------------------------------------------------------------------------
import ClassyPrelude hiding (empty)
import Control.Monad.State  (get, put)
import Data.Acid            (Update, Query, makeAcidic)
import Data.IxSet
import Data.SafeCopy        (deriveSafeCopy, base)
import Model
--------------------------------------------------------------------------------

data Database = Database { competitors :: IxSet Competitor }

instance Indexable Competitor where
  empty = ixSet [ ixFun $ \Competitor{..} -> [ competitorId ]
                , ixFun $ \Competitor{..} -> map resultDivision competitorResults
                ]

--------------------------------------------------------------------------------
deriveSafeCopy 0 'base ''Database
deriveSafeCopy 0 'base ''Competitor
deriveSafeCopy 0 'base ''Result
deriveSafeCopy 0 'base ''Competition
deriveSafeCopy 0 'base ''Event
deriveSafeCopy 0 'base ''Division
deriveSafeCopy 0 'base ''Placement
deriveSafeCopy 0 'base ''Role
--------------------------------------------------------------------------------
deriveSafeCopy 0 'base ''CompetitorId
--------------------------------------------------------------------------------

initDatabase :: Database
initDatabase = Database { competitors = empty }

--------------------------------------------------------------------------------

insertCompetitor :: Competitor -> Update Database ()
insertCompetitor competitor@Competitor{..} = do
  db@Database{..} <- get
  put $ db { competitors = updateIx competitorId competitor competitors}

getCompetitor :: Integer -> Query Database (Maybe Competitor)
getCompetitor competitorId = fmap (getOne . getEQ (CompetitorId competitorId) . competitors) ask

--------------------------------------------------------------------------------
makeAcidic ''Database [ 'getCompetitor
                      , 'insertCompetitor
                      ]
