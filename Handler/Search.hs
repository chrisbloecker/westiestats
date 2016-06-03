module Handler.Search
  where
--------------------------------------------------------------------------------
import           Database
import           Import
import           Model
import           Yesod.Form.Bootstrap3      (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)
--------------------------------------------------------------------------------

getSearchR :: Handler Html
getSearchR = do
    (searchWidget, searchEnctype) <- generateFormPost searchForm
    let mcompetitor = Nothing
    defaultLayout $(widgetFile "search")

postSearchR :: Handler Html
postSearchR = do
    ((searchResult, searchWidget), searchEnctype) <- runFormPost searchForm
    case searchResult of
        FormSuccess wscid -> do
            mcompetitor <- acidQuery (GetCompetitor (fromIntegral wscid))
            defaultLayout $(widgetFile "search")
        _ -> redirect SearchR

searchForm :: Form Int
searchForm = renderBootstrap3 BootstrapBasicForm $
    areq intField (withSmallInput "wscid ") Nothing
