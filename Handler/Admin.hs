module Handler.Admin
  where
--------------------------------------------------------------------------------
import           Competitor
import           Database
import           Import
import           Yesod.Form.Bootstrap3      (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)
--------------------------------------------------------------------------------
import qualified Data.Text             as T
--------------------------------------------------------------------------------

getAdminR :: Handler Html
getAdminR = do
  (loadWidget, loadEnctype) <- generateFormPost loadForm

  defaultLayout $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
  ((loadResult, loadWidget), loadEnctype) <- runFormPost loadForm
  case loadResult of
    FormSuccess (from, to) -> do
      forM_ [from .. to] $ \wscid -> do
        mcompetitor <- liftIO $ loadCompetitor (fromIntegral wscid)
        case mcompetitor of
          Left err -> $(logInfo) ("No entry for id " `T.append` (pack . show $ wscid) `T.append` ":" `T.append` (pack . show $ err))
          Right (competitor, eventDetails) -> do
            acidUpdate (InsertCompetitor competitor)
            forM_ eventDetails $ \e -> acidUpdate (InsertEventDetails e)
      redirect AdminR
    _ -> error "Something went wrong :("

loadForm :: Form (Int, Int)
loadForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq intField (withSmallInput "from ") Nothing
    <*> areq intField (withSmallInput "to ") Nothing
