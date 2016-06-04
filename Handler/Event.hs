module Handler.Event
  where
--------------------------------------------------------------------------------
import           Data.Maybe    (fromJust)
import           Database
import           Import
import           Model
--------------------------------------------------------------------------------
import qualified Data.Set as S (filter)
import qualified Data.Map as M (keys, lookup)
--------------------------------------------------------------------------------

getEventR :: EventId -> EventYear -> Handler Html
getEventR eventId eventYear = do
  meventDetails <- acidQuery (GetEventDetails eventId)
  case meventDetails of
    Nothing -> defaultLayout [whamlet|Not found :(|]
    Just EventDetails{..} -> do
      let thisYear  = fromJust $ M.lookup eventYear eventDetailsResults
          years     = reverse . sort . M.keys $ eventDetailsResults
          divisions = M.keys thisYear
      defaultLayout $(widgetFile "event")

  where
    getEntry :: Division -> Placement -> Role -> Map Division (Set ResultEntry) -> [ResultEntry]
    getEntry d p r m = toList . S.filter (\(_,_,r',p') -> r' == r && p' == p) . fromJust . lookup d $ m

getEventListR :: Handler Html
getEventListR = do
  events <- sortOn eventDetailsName <$> acidQuery GetEvents
  defaultLayout $(widgetFile "event_list")
