{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Competitor
  ( loadDancer
  , loadCompetitor
  , extractEventDetails
  , getPointsAsIn
  ) where
--------------------------------------------------------------------------------
import           CBPrelude
import           ClassyPrelude
import           Import.DeriveJSON
import           Model
import           Network.HTTP.Conduit        (Manager, Request (..), httpLbs, parseRequest, responseBody)
--------------------------------------------------------------------------------
import qualified Data.Map              as M  (singleton)
import qualified Data.Set              as S  (singleton)
import qualified Data.Text             as T  (pack)
import qualified Model.External        as E
import qualified Data.ByteString.Char8 as BS (pack)
--------------------------------------------------------------------------------

loadDancer :: Manager -> WscId -> IO (Either String E.Person)
loadDancer manager (WscId wscid) = do
  putStrLn $ "Loading " ++ T.pack (show wscid)
  req      <- parseRequest "https://points.worldsdc.com/lookup/find"
  response <- flip httpLbs manager $ req { method      = "POST"
                                         , queryString = "num=" ++ (BS.pack . show $ wscid)
                                         }
  return . eitherDecode' . responseBody $ response

loadCompetitor :: Manager -> WscId -> IO (Either String (Competitor, [EventDetails]))
loadCompetitor manager wscid = do
  mp <- loadDancer manager wscid
  return $ fmap ((id &&& extractEventDetails) . fromPerson) mp

extractEventDetails :: Competitor -> [EventDetails]
extractEventDetails Competitor{..} =
  concatMap (\Result{..} ->
        map (\Competition{..} -> EventDetails { eventDetailsId       = eventId       competitionEvent
                                              , eventDetailsName     = eventName     competitionEvent
                                              , eventDetailsLocation = eventLocation competitionEvent
                                              , eventDetailsResults  = M.singleton (eventYear competitionEvent)
                                                                                   (M.singleton resultDivision (S.singleton (competitorWscId, unwords [competitorFirstName, competitorLastName], competitionRole, competitionPlacement)))
                                              }
            ) resultCompetitions
            ) competitorResults

getPointsAsIn :: Role -> Division -> Competitor -> Maybe ResultPoints
getPointsAsIn role division Competitor{..} =
  case filter (resultDivision `is` division) competitorResults of
    []             -> Nothing
    (Result{..}:_) -> Just . ResultPoints . sum . map competitionPoints . filter (competitionRole `is` role) $ resultCompetitions
