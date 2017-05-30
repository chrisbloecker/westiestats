module Handler.AutoComplete
  where
--------------------------------------------------------------------------------
import Data.Text       (toLower)
import Database
import Import   hiding (toLower)
import Model
--------------------------------------------------------------------------------

getAutoCompleteR :: Handler Value
getAutoCompleteR = do
  mquery <- fmap toLower <$> lookupGetParam "query"
  case mquery of
    Nothing    -> return . toJSON $ Suggestions []
    Just query ->          toJSON . Suggestions . map mkSuggestion <$> acidQuery (SearchPrefix $ Prefix query)
