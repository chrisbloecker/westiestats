module Handler.Competitor
  where
--------------------------------------------------------------------------------
import Database
import Data.List ((!!))
import Data.Text (splitOn, strip)
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

  let areas = map (toArea . eventLocation . competitionEvent) . concatMap resultCompetitions . competitorResults <$> mcompetitor

  defaultLayout $(widgetFile "competitor")

    where
      toArea :: Text -> Country
      toArea = toCountry . strip . (!! 1) . splitOn ","

      toCountry :: Text -> Country
      toCountry "Australia" = Country "AU"
      toCountry "Finnland"  = Country "FI"
      toCountry "Germany"   = Country "DE"
      toCountry "Norway"    = Country "NO"
      toCountry "Russia"    = Country "RU"
      toCountry "Sweden"    = Country "SE"
      toCountry t           = Country t
