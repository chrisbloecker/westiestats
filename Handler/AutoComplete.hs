module Handler.AutoComplete
  where
--------------------------------------------------------------------------------
import Database
import Import
import Model
--------------------------------------------------------------------------------

getAutoCompleteR :: Handler Value
getAutoCompleteR = do
  mquery <- lookupGetParam "query"
  case mquery of
    Nothing    -> return . toJSON $ Suggestions []
    Just query ->          toJSON . Suggestions . map mkSuggestion <$> acidQuery (SearchPrefix $ Prefix query)
