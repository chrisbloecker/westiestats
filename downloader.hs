{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import           Control.Concurrent.ParallelIO
import           ClassyPrelude
import           Competitor                    (loadDancer)
import           Data.Aeson                    (encode)
import           Data.Either                   (rights)
import           Data.Time.Clock               (getCurrentTime, utctDay)
import           Model                         (WscId (..))
import           Model.External                (Snapshot (..))
import           Options.Applicative
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS    (writeFile)
--------------------------------------------------------------------------------

data Options = Options { from :: Integer
                       , to   :: Integer
                       , out  :: String
                       }

options :: Parser Options
options = Options
  <$> option auto ( long "from"
              <> metavar "FROM"
              <> help "first wscid to retrieve"
                )
  <*> option auto ( long "to"
              <> metavar "TO"
              <> help "last wscid to retrieve"
                )
  <*> strOption ( long "out"
              <> metavar "OUT"
              <> help "path of the output file"
                )

--------------------------------------------------------------------------------

run :: Options -> IO ()
run Options{..} = do
  let snapshotFromWscId = from
      snapshotToWscId   = to
  snapshotDate    <- utctDay <$> getCurrentTime
  snapshotPersons <- rights <$> parallel [loadDancer (WscId wscid) | wscid <- [from .. to]]
  BS.writeFile out (encode Snapshot{..})

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "Download for wsdc data"
      <> header ""
      )
