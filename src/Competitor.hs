module Competitor
  ( loadDancer
  , loadCompetitor
  , getPointsAsIn
  ) where
--------------------------------------------------------------------------------
import           CBPrelude
import           ClassyPrelude
import           Data.Aeson                 (eitherDecode')
import           Data.Text             as T (pack, replace)
import           Model
import           Network.HTTP               (simpleHTTP, postRequest, getResponseBody)
--------------------------------------------------------------------------------
import qualified Data.Set              as S (singleton)
import qualified Data.Map              as M (singleton)
import qualified Model.External        as E
--------------------------------------------------------------------------------

url :: Int -> String
url n = "http://wsdc-points.us-west-2.elasticbeanstalk.com/lookup/find?num=" ++ show n

loadDancer :: WscId -> IO (Either String E.Person)
loadDancer (WscId wscid) = do
  body <- getResponseBody =<< simpleHTTP (postRequest $ url (fromIntegral wscid))
  return . eitherDecode' . encodeUtf8 . fromStrict . (T.replace ",\"placements\":[]" "" :: Text -> Text) . T.pack $ body

loadCompetitor :: WscId -> IO (Either String (Competitor, [EventDetails]))
loadCompetitor wscid = do
  mp <- (loadDancer wscid)
  return $ fmap ((id &&& extractEventDetails) . fromPerson) mp
--loadCompetitor wscid = fmap ((id &&& extractEventDetails) . fromPerson) (loadDancer wscid)

extractEventDetails :: Competitor -> [EventDetails]
extractEventDetails Competitor{..} =
  concatMap (\Result{..} ->
        map (\Competition{..} -> EventDetails { eventDetailsId       = eventId       competitionEvent
                                              , eventDetailsName     = eventName     competitionEvent
                                              , eventDetailsLocation = eventLocation competitionEvent
                                              , eventDetailsResults  = M.singleton (eventYear competitionEvent)
                                                                                   (M.singleton resultDivision (S.singleton (competitorWscId, competitorName, competitionRole, competitionPlacement)))
                                              }
            ) resultCompetitions
            ) competitorResults

getPointsAsIn :: Role -> Division -> Competitor -> Maybe ResultPoints
getPointsAsIn role division Competitor{..} =
  case filter (resultDivision `is` division) competitorResults of
    []             -> Nothing
    (Result{..}:_) -> Just . ResultPoints . sum . map competitionPoints . filter (competitionRole `is` role) $ resultCompetitions
