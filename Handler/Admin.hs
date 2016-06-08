module Handler.Admin
  where
--------------------------------------------------------------------------------
import           Competitor                 (extractEventDetails)
import           Database
import           Data.Conduit.Binary
import           Import
import           Import.DeriveJSON
import           Model                      (Competitor, fromPerson)
import           Yesod.Form.Bootstrap3      (BootstrapFormLayout (..), renderBootstrap3)
--------------------------------------------------------------------------------

getAdminR :: Handler Html
getAdminR = do
  (loadWidget, loadEnctype) <- generateFormPost loadForm
  defaultLayout $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
  ((loadResult, _), _) <- runFormPost loadForm
  case loadResult of
    FormSuccess fileInfo -> do
      fileBytes <- runResourceT $ fileSource fileInfo $$ sinkLbs
      let mpersons = fmap fromPerson <$> eitherDecode' fileBytes :: Either String [Competitor]
      case mpersons of
        Left err      -> error $ pack err
        Right persons -> do
          acidUpdates (map InsertCompetitor   persons)
          acidUpdates (map InsertEventDetails (concatMap extractEventDetails persons))
      redirect AdminR
    _ -> error "Something went wrong :("

loadForm :: Form FileInfo
loadForm = renderBootstrap3 BootstrapInlineForm $
     fileAFormReq "Choose a file"
