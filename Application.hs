{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    ) where
--------------------------------------------------------------------------------
import Control.Monad.Logger                 (liftLoc)
import Import                        hiding ((.:))
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings, defaultShouldDisplayException, runSettings, setHost, setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (..), OutputFormat (..), destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet, toLogStr)
--------------------------------------------------------------------------------
import Competitor                           (extractEventDetails)
import Database
import Data.Acid
import Data.Acid.Advanced
import Data.JsonStream.Parser               ((.:))
--import Import.DeriveJSON                    (eitherDecode')
import Model                                (fromPerson)
import Model.External                       (day, person)
import System.Directory                     (getDirectoryContents)
--------------------------------------------------------------------------------
import Handler.AutoComplete
import Handler.Comment
import Handler.Common
import Handler.Competitor
import Handler.Event
import Handler.Home
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy   as BS     (readFile)
import qualified Data.List              as L      (maximum, head)
import qualified Data.JsonStream.Parser as Stream (arrayOf, parseLazyByteString)
--------------------------------------------------------------------------------

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    getDatabase <- openLocalState initDatabase

    snapshotFile <- ("./data/" ++) . L.maximum <$> getDirectoryContents "./data/"
    putStrLn $ "[DEBUG] Loading snapshot file from " ++ pack snapshotFile
    json <- BS.readFile snapshotFile

    let snapshotDate = L.head . Stream.parseLazyByteString ("snapshotDate" .: day) $ json
    putStrLn $ "[DEBUG] Snapshot date of snapshot file is " ++ pack (show snapshotDate)

    databaseDate <- query getDatabase GetSnapshotDate
    putStrLn $ "[DEBUG] Snapshot date of database is " ++ pack (show databaseDate)

    -- We only build up the database if the snapshot is newer
    when (databaseDate < snapshotDate) $ do
      putStrLn "[DEBUG] Importing snapshot file"
      let snapshotPersons = Stream.parseLazyByteString ("snapshotPersons" .: Stream.arrayOf person) json
      groupUpdates getDatabase [SetSnapshotDate snapshotDate]
      forM_ snapshotPersons $ \person -> do
        let competitor = fromPerson person
        groupUpdates getDatabase [InsertCompetitor competitor]
        groupUpdates getDatabase [InsertEventDetails event | event <- extractEventDetails competitor]
      createCheckpoint getDatabase
      createArchive getDatabase
      putStrLn "[DEBUG] Import complete"

    putStrLn "[DEBUG] Database loaded"

    {-
    let msnapshot = eitherDecode' json :: Either String Snapshot
    case msnapshot of
      Left err -> error $ pack err
      Right Snapshot{..} -> do
        oldDate <- query getDatabase GetSnapshotDate
        -- We only build up the database if the snapshot is newer
        when (oldDate < snapshotDate) $ do
          putStrLn $ unwords ["[INFO] The old snapshot was from", pack . show $ oldDate, "but there is a new version from", pack . show $ snapshotDate]
          groupUpdates getDatabase [SetSnapshotDate snapshotDate]
          forM_ snapshotPersons $ \person -> do
            let competitor = fromPerson person
            groupUpdates getDatabase [InsertCompetitor competitor]
            groupUpdates getDatabase [InsertEventDetails event | event <- extractEventDetails competitor]
          createCheckpoint getDatabase
          createArchive getDatabase
    -}

    -- Return the foundation
    return App {..}

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h
