module Import.DeriveJSON
  ( FromJSON, decode', eitherDecode'
  , deriveJSON, jsonOptions
  , Text
  , encodeUtf8
  , fieldLabel
  ) where

-------------------------------------------------------------------------------
import ClassyPrelude
import Data.Aeson    (FromJSON, decode', eitherDecode')
import Data.Aeson.TH (Options (..), SumEncoding (..), deriveJSON)
-------------------------------------------------------------------------------

fieldLabel :: String -> String
-- person
fieldLabel "personType"           = "type"
fieldLabel "personDancer"         = "dancer"
fieldLabel "personPlacements"     = "placements"
-- dancer
fieldLabel "dancerId"             = "id"
fieldLabel "dancerFirstName"      = "first_name"
fieldLabel "dancerLastName"       = "last_name"
fieldLabel "dancerWscid"          = "wscid"
-- placements
fieldLabel "westCoastSwing"       = "West Coast Swing"
fieldLabel "lindy"                = "Lindy"
-- division
fieldLabel "divisionDetails"      = "division"
fieldLabel "divisionTotalPoints"  = "total_points"
fieldLabel "divisionCompetitions" = "competitions"
-- details
fieldLabel "detailsId"            = "id"
fieldLabel "detailsName"          = "name"
fieldLabel "detailsAbbreviation"  = "abbreviation"
-- competition
fieldLabel "competitionRole"      = "role"
fieldLabel "competitionPoints"    = "points"
fieldLabel "competitionEvent"     = "event"
fieldLabel "competitionResult"    = "result"
-- event
fieldLabel "eventId"              = "id"
fieldLabel "eventName"            = "name"
fieldLabel "eventLocation"        = "location"
fieldLabel "eventUrl"             = "url"
fieldLabel "eventDate"            = "date"
-- all the rest
fieldLabel s = s

jsonOptions :: Options
jsonOptions = Options { fieldLabelModifier      = fieldLabel
                      , constructorTagModifier  = id
                      , allNullaryToStringTag   = False
                      , omitNothingFields       = True
                      , sumEncoding             = ObjectWithSingleField
                      , unwrapUnaryRecords      = False
                      }
