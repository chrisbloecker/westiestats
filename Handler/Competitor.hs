module Handler.Competitor
  where
--------------------------------------------------------------------------------
import Database
import Import
import Model
--------------------------------------------------------------------------------

postSearchR :: Handler Html
postSearchR = do
  mwscid <- runInputPost $ fmap WscId <$> iopt intField "wscid"
  redirect $ case mwscid of
    Nothing    -> HomeR
    Just wscid -> CompetitorR wscid


getCompetitorR :: WscId -> Handler Html
getCompetitorR wscid = do
  mcompetitor <- acidQuery (GetCompetitor wscid)
  defaultLayout $(widgetFile "competitor")
