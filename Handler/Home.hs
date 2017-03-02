module Handler.Home
  where
--------------------------------------------------------------------------------
import Import
--------------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = redirect (EventListR, [("start", "A")]) {-do
  mostPointsLeader <- forM [Newcomer .. Teacher] $ \d -> take 15 <$> acidQuery (GetMostPoints Leader d)
  let divisionsAndMostPoints = zip [Newcomer .. Teacher] mostPointsLeader
  defaultLayout $(widgetFile "homepage")-}
