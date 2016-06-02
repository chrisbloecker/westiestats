module Handler.Search where

import Import
import Model
import Data.Aeson            (eitherDecode')
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Network.HTTP          (simpleHTTP, postRequest, getResponseBody)
import Data.Text as T        (pack, replace)

getSearchR :: Handler Html
getSearchR = do
    (searchWidget, searchEnctype) <- generateFormPost searchForm
    let result = Left "No result" :: Either String Person
    defaultLayout $ do
        $(widgetFile "search")

postSearchR :: Handler Html
postSearchR = do
    ((searchResult, searchWidget), searchEnctype) <- runFormPost searchForm
    case searchResult of
        FormSuccess wscid -> do
            result <- getPerson wscid
            defaultLayout $ do
                $(widgetFile "search")
        _ -> redirect SearchR

searchForm :: Form Int
searchForm = renderBootstrap3 BootstrapBasicForm $
    areq intField (withSmallInput "wscid") Nothing

url :: Int -> String
url n = "http://wsdc-points.us-west-2.elasticbeanstalk.com/lookup/find?num=" ++ show n

getPerson :: Int -> Handler (Either String Person)
getPerson n = do
  response <- liftIO $ simpleHTTP (postRequest $ url n)
  body     <- liftIO $ getResponseBody response
  let mperson = eitherDecode' . encodeUtf8 . fromStrict . (T.replace ",\"placements\":[]" "" :: Text -> Text) . T.pack $ body :: Either String Person
  return mperson
