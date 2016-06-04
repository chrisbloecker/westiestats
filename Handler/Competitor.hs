module Handler.Competitor
  where
--------------------------------------------------------------------------------
import Database
import Import
import Model
--------------------------------------------------------------------------------

getCompetitorR :: Handler Html
getCompetitorR = do
  wscid <- runInputGet $ WscId <$> ireq intField "wscid"
  mcompetitor <- acidQuery (GetCompetitor wscid)
  defaultLayout $(widgetFile "competitor")
