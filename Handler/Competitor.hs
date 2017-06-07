module Handler.Competitor
  where
--------------------------------------------------------------------------------
import Database
import Data.List (nub)
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

  let locations = map (eventLocation . competitionEvent) . concatMap resultCompetitions . competitorResults <$> mcompetitor
      countries = fromMaybe [] $ nub . mapMaybe locationCountry                                                   <$> locations
      states    = fromMaybe [] $ nub . mapMaybe locationState   . filter (locationCountry `is` Just UnitedStates) <$> locations

  defaultLayout $(widgetFile "competitor")
