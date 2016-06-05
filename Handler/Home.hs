module Handler.Home
  where
--------------------------------------------------------------------------------
import Database
import Import
import Model
import Text.Julius (RawJS (..))
--------------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
  mostPointsLeader <- forM [Newcomer .. Teacher] $ \d -> take 15 <$> acidQuery (GetMostPoints Leader d)
  let divisionsAndMostPoints = zip [Newcomer .. Teacher] mostPointsLeader
  defaultLayout $(widgetFile "homepage")
