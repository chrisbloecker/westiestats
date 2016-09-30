module Handler.Event
  where
--------------------------------------------------------------------------------
import           Data.Maybe    (fromJust)
import           Database
import           Import
import           Model
--------------------------------------------------------------------------------
import qualified Data.Set  as S (filter)
import qualified Data.Text as T (isPrefixOf)
import qualified Data.Map  as M (keys, lookup)
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
  start <- runInputGet (ireq textField "start")
  events <- sortOn eventDetailsName . filterEvents start <$> acidQuery GetEvents

  let starts = ["0-9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"] :: [Text]

  defaultLayout $(widgetFile "event_list")

    where
      filterEvents :: Text -> [EventDetails] -> [EventDetails]
      filterEvents t = let starts = case t of
                                      ""    -> ["A"]
                                      "0-9" -> ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
                                      _     -> [t]
                       in filter (\EventDetails{..} -> any (flip T.isPrefixOf eventDetailsName) starts)
