module Competitor
  ( loadCompetitor
  ) where
--------------------------------------------------------------------------------
import           Import
import           Data.Aeson                 (eitherDecode')
import           Data.Text             as T (pack, replace)
import           Model
import           Network.HTTP               (simpleHTTP, postRequest, getResponseBody)
--------------------------------------------------------------------------------
import qualified Model.External        as E
--------------------------------------------------------------------------------

url :: Int -> String
url n = "http://wsdc-points.us-west-2.elasticbeanstalk.com/lookup/find?num=" ++ show n

loadCompetitor :: Int -> IO (Either String Competitor)
loadCompetitor n = do
  body <- getResponseBody =<< simpleHTTP (postRequest $ url n)
  let mcompetitor = eitherDecode' . encodeUtf8 . fromStrict . (T.replace ",\"placements\":[]" "" :: Text -> Text) . T.pack $ body :: Either String E.Person
  return . fmap fromPerson $ mcompetitor
