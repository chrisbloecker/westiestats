module Handler.Search
  where
--------------------------------------------------------------------------------
import           Database
import           Import
import           Model
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
            mcompetitor <- acidQuery (GetCompetitor wscid)
            defaultLayout $(widgetFile "search")
        _ -> redirect SearchR
