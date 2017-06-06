module Model.Location
  where

import ClassyPrelude
import Data.Aeson
import Data.Attoparsec.Text
import Text.Blaze           (ToMarkup (..))

data Location = Location { locationText    :: !Text
                         , locationCity    :: !(Maybe City)
                         , locationState   :: !(Maybe State)
                         , locationCountry :: !(Maybe Country)
                         }
  deriving (Eq, Ord, Show)

instance ToMarkup Location where
  toMarkup Location{..} = toMarkup locationText

newtype City = City { unCity :: Text } deriving (Eq, Ord, Show)

data Country = Australia
             | Austria
             | Brazil
             | Canada
             | England
             | France
             | Finland
             | Germany
             | Hungary
             | Israel
             | Italy
             | Latvia
             | Netherlands
             | NewZealand
             | Norway
             | Poland
             | Russia
             | Scotland
             | Singapore
             | SouthKorea
             | Spain
             | Sweden
             | Switzerland
             | Ukraine
             | UnitedKingdom
             | UnitedStates
  deriving (Eq, Ord, Show)

instance ToJSON Country where
  toJSON Australia     = object [ "id" .= ("AU" :: Text) ]
  toJSON Austria       = object [ "id" .= ("AT" :: Text) ]
  toJSON Brazil        = object [ "id" .= ("BR" :: Text) ]
  toJSON Canada        = object [ "id" .= ("CA" :: Text) ]
  toJSON England       = object [ "id" .= ("GB" :: Text) ]
  toJSON France        = object [ "id" .= ("FR" :: Text) ]
  toJSON Finland       = object [ "id" .= ("FI" :: Text) ]
  toJSON Germany       = object [ "id" .= ("DE" :: Text) ]
  toJSON Hungary       = object [ "id" .= ("HU" :: Text) ]
  toJSON Israel        = object [ "id" .= ("IL" :: Text) ]
  toJSON Italy         = object [ "id" .= ("IT" :: Text) ]
  toJSON Latvia        = object [ "id" .= ("LV" :: Text) ]
  toJSON Netherlands   = object [ "id" .= ("NL" :: Text) ]
  toJSON NewZealand    = object [ "id" .= ("NZ" :: Text) ]
  toJSON Norway        = object [ "id" .= ("NO" :: Text) ]
  toJSON Poland        = object [ "id" .= ("PL" :: Text) ]
  toJSON Russia        = object [ "id" .= ("RU" :: Text) ]
  toJSON Scotland      = object [ "id" .= ("GB" :: Text) ]
  toJSON Singapore     = object [ "id" .= ("SG" :: Text) ]
  toJSON SouthKorea    = object [ "id" .= ("KR" :: Text) ]
  toJSON Spain         = object [ "id" .= ("ES" :: Text) ]
  toJSON Sweden        = object [ "id" .= ("SE" :: Text) ]
  toJSON Switzerland   = object [ "id" .= ("CH" :: Text) ]
  toJSON Ukraine       = object [ "id" .= ("UA" :: Text) ]
  toJSON UnitedKingdom = object [ "id" .= ("GB" :: Text) ]
  toJSON UnitedStates  = object [ "id" .= ("US" :: Text) ]

data State = -- states in the United States
             Alabama            -- AL
           | Alaska             -- AK
           | Arizona            -- AZ
           | Arkansas           -- AR
           | California         -- CA
           | Colorado           -- CO
           | Connecticut        -- CT
           | DistrictOfColumbia -- DC
           | Delaware           -- DE
           | Florida            -- FL
           | Georgia            -- GA
           | Hawaii             -- HI
           | Idaho              -- ID
           | Illinois           -- IL
           | Indiana            -- IN
           | Iowa               -- IA
           | Kansas             -- KS
           | Kentucky           -- KY
           | Louisiana          -- LA
           | Maine              -- ME
           | Maryland           -- MD
           | Massachusetts      -- MA
           | Michigan           -- MI
           | Minnesota          -- MN
           | Mississippi        -- MS
           | Montana            -- MO
           | Nebraska           -- NE
           | Nevada             -- NV
           | NewHampshire       -- NH
           | NewJersey          -- NJ
           | NewMexico          -- NM
           | NewYork            -- NY
           | NorthCarolina      -- NC
           | NorthDakota        -- ND
           | Ohio               -- OH
           | Oklahoma           -- OK
           | Oregon             -- OR
           | Pennsylvania       -- PA
           | RhodeIsland        -- RI
           | SouthCarolina      -- SC
           | SouthDakota        -- SD
           | Tennessee          -- TN
           | Texas              -- TX
           | Utah               -- UT
           | Vermont            -- VT
           | Virginia           -- VA
           | Washington         -- WA
           | WestVirginia       -- WV
           | Wisconsin          -- WI
           | Wyoming            -- WY

             -- states in Canada
           | Alberta            -- AB
           | BritishColumbia    -- BC
           | NewBrunswick       -- NB
           | Newfoundland       -- NL
           | NovaScotia         -- NS
           | Manitoba           -- MB
           | Ontario            -- ON
           | PrinceEdwardIsland -- PE
           | Quebec             -- QC
           | Saskatchewan       -- SK
  deriving (Eq, Ord, Show)

instance ToJSON State where
  toJSON Alabama            = object [ "id" .= ("US-AL" :: Text) ]
  toJSON Alaska             = object [ "id" .= ("US-AK" :: Text) ]
  toJSON Arizona            = object [ "id" .= ("US-AZ" :: Text) ]
  toJSON Arkansas           = object [ "id" .= ("US-AR" :: Text) ]
  toJSON California         = object [ "id" .= ("US-CA" :: Text) ]
  toJSON Colorado           = object [ "id" .= ("US-CO" :: Text) ]
  toJSON Connecticut        = object [ "id" .= ("US-CT" :: Text) ]
  toJSON DistrictOfColumbia = object [ "id" .= ("US-DC" :: Text) ]
  toJSON Delaware           = object [ "id" .= ("US-DE" :: Text) ]
  toJSON Florida            = object [ "id" .= ("US-FL" :: Text) ]
  toJSON Georgia            = object [ "id" .= ("US-GA" :: Text) ]
  toJSON Hawaii             = object [ "id" .= ("US-HI" :: Text) ]
  toJSON Idaho              = object [ "id" .= ("US-ID" :: Text) ]
  toJSON Illinois           = object [ "id" .= ("US-IL" :: Text) ]
  toJSON Indiana            = object [ "id" .= ("US-IN" :: Text) ]
  toJSON Iowa               = object [ "id" .= ("US-IA" :: Text) ]
  toJSON Kansas             = object [ "id" .= ("US-KS" :: Text) ]
  toJSON Kentucky           = object [ "id" .= ("US-KY" :: Text) ]
  toJSON Louisiana          = object [ "id" .= ("US-LA" :: Text) ]
  toJSON Maine              = object [ "id" .= ("US-ME" :: Text) ]
  toJSON Maryland           = object [ "id" .= ("US-MD" :: Text) ]
  toJSON Massachusetts      = object [ "id" .= ("US-MA" :: Text) ]
  toJSON Michigan           = object [ "id" .= ("US-MI" :: Text) ]
  toJSON Minnesota          = object [ "id" .= ("US-MN" :: Text) ]
  toJSON Mississippi        = object [ "id" .= ("US-MS" :: Text) ]
  toJSON Montana            = object [ "id" .= ("US-MO" :: Text) ]
  toJSON Nebraska           = object [ "id" .= ("US-NE" :: Text) ]
  toJSON Nevada             = object [ "id" .= ("US-NV" :: Text) ]
  toJSON NewHampshire       = object [ "id" .= ("US-NH" :: Text) ]
  toJSON NewJersey          = object [ "id" .= ("US-NJ" :: Text) ]
  toJSON NewMexico          = object [ "id" .= ("US-NM" :: Text) ]
  toJSON NewYork            = object [ "id" .= ("US-NY" :: Text) ]
  toJSON NorthCarolina      = object [ "id" .= ("US-NC" :: Text) ]
  toJSON NorthDakota        = object [ "id" .= ("US-ND" :: Text) ]
  toJSON Ohio               = object [ "id" .= ("US-OH" :: Text) ]
  toJSON Oklahoma           = object [ "id" .= ("US-OK" :: Text) ]
  toJSON Oregon             = object [ "id" .= ("US-OR" :: Text) ]
  toJSON Pennsylvania       = object [ "id" .= ("US-PA" :: Text) ]
  toJSON RhodeIsland        = object [ "id" .= ("US-RI" :: Text) ]
  toJSON SouthCarolina      = object [ "id" .= ("US-SC" :: Text) ]
  toJSON SouthDakota        = object [ "id" .= ("US-SD" :: Text) ]
  toJSON Tennessee          = object [ "id" .= ("US-TN" :: Text) ]
  toJSON Texas              = object [ "id" .= ("US-TX" :: Text) ]
  toJSON Utah               = object [ "id" .= ("US-UT" :: Text) ]
  toJSON Vermont            = object [ "id" .= ("US-VT" :: Text) ]
  toJSON Virginia           = object [ "id" .= ("US-VA" :: Text) ]
  toJSON Washington         = object [ "id" .= ("US-WA" :: Text) ]
  toJSON WestVirginia       = object [ "id" .= ("US-WV" :: Text) ]
  toJSON Wisconsin          = object [ "id" .= ("US-WI" :: Text) ]
  toJSON Wyoming            = object [ "id" .= ("US-WY" :: Text) ]
  toJSON Alberta            = object [ "id" .= ("US-AB" :: Text) ]
  toJSON BritishColumbia    = object [ "id" .= ("US-BC" :: Text) ]
  toJSON NewBrunswick       = object [ "id" .= ("US-NB" :: Text) ]
  toJSON Newfoundland       = object [ "id" .= ("US-NL" :: Text) ]
  toJSON NovaScotia         = object [ "id" .= ("US-NS" :: Text) ]
  toJSON Manitoba           = object [ "id" .= ("US-MB" :: Text) ]
  toJSON Ontario            = object [ "id" .= ("US-ON" :: Text) ]
  toJSON PrinceEdwardIsland = object [ "id" .= ("US-PE" :: Text) ]
  toJSON Quebec             = object [ "id" .= ("US-QC" :: Text) ]
  toJSON Saskatchewan       = object [ "id" .= ("US-SK" :: Text) ]

countryFromState :: State -> Country
countryFromState Alabama            = UnitedStates
countryFromState Alaska             = UnitedStates
countryFromState Arizona            = UnitedStates
countryFromState Arkansas           = UnitedStates
countryFromState California         = UnitedStates
countryFromState Colorado           = UnitedStates
countryFromState Connecticut        = UnitedStates
countryFromState DistrictOfColumbia = UnitedStates
countryFromState Delaware           = UnitedStates
countryFromState Florida            = UnitedStates
countryFromState Georgia            = UnitedStates
countryFromState Hawaii             = UnitedStates
countryFromState Idaho              = UnitedStates
countryFromState Illinois           = UnitedStates
countryFromState Indiana            = UnitedStates
countryFromState Iowa               = UnitedStates
countryFromState Kansas             = UnitedStates
countryFromState Kentucky           = UnitedStates
countryFromState Louisiana          = UnitedStates
countryFromState Maine              = UnitedStates
countryFromState Maryland           = UnitedStates
countryFromState Massachusetts      = UnitedStates
countryFromState Michigan           = UnitedStates
countryFromState Minnesota          = UnitedStates
countryFromState Mississippi        = UnitedStates
countryFromState Montana            = UnitedStates
countryFromState Nebraska           = UnitedStates
countryFromState Nevada             = UnitedStates
countryFromState NewHampshire       = UnitedStates
countryFromState NewJersey          = UnitedStates
countryFromState NewMexico          = UnitedStates
countryFromState NewYork            = UnitedStates
countryFromState NorthCarolina      = UnitedStates
countryFromState NorthDakota        = UnitedStates
countryFromState Ohio               = UnitedStates
countryFromState Oklahoma           = UnitedStates
countryFromState Oregon             = UnitedStates
countryFromState Pennsylvania       = UnitedStates
countryFromState RhodeIsland        = UnitedStates
countryFromState SouthCarolina      = UnitedStates
countryFromState SouthDakota        = UnitedStates
countryFromState Tennessee          = UnitedStates
countryFromState Texas              = UnitedStates
countryFromState Utah               = UnitedStates
countryFromState Vermont            = UnitedStates
countryFromState Virginia           = UnitedStates
countryFromState Washington         = UnitedStates
countryFromState WestVirginia       = UnitedStates
countryFromState Wisconsin          = UnitedStates
countryFromState Wyoming            = UnitedStates
countryFromState Alberta            = Canada
countryFromState BritishColumbia    = Canada
countryFromState NewBrunswick       = Canada
countryFromState Newfoundland       = Canada
countryFromState NovaScotia         = Canada
countryFromState Manitoba           = Canada
countryFromState Ontario            = Canada
countryFromState PrinceEdwardIsland = Canada
countryFromState Quebec             = Canada
countryFromState Saskatchewan       = Canada


parseLocation :: Text -> Location
parseLocation t = case parseOnly location t of
  Left  _   -> Location t Nothing Nothing Nothing
  Right loc -> loc { locationText = t }

location :: Parser Location
location = cityAndCountry <|> cityAndState <|> justCountry
  where
    cityAndCountry :: Parser Location
    cityAndCountry = do
      skipSpace
      locationCity <- city
      skipSpace
      _ <- char ','
      skipSpace
      locationCountry <- country
      skipSpace
      b <- atEnd
      unless b $ fail "Unconsumed input"
      let locationState = Nothing
          locationText  = ""
      return Location{..}

    cityAndState :: Parser Location
    cityAndState = do
      skipSpace
      locationCity <- city
      skipSpace
      _ <- char ','
      skipSpace
      locationState <- state
      skipSpace
      b <- atEnd
      unless b $ fail "Unconsumed input"
      let locationCountry = countryFromState <$> locationState
          locationText    = ""
      return Location{..}

    justCountry :: Parser Location
    justCountry = do
      skipSpace
      locationCountry <- country
      skipSpace
      b <- atEnd
      unless b $ fail $ "Unconsumed input"
      let locationState = Nothing
          locationCity  = Nothing
          locationText  = ""
      return Location{..}


country :: Parser (Maybe Country)
country = (string "Australia"      >> return (Just Australia))
      <|> (string "Austria"        >> return (Just Austria))
      <|> (string "Brazil"         >> return (Just Brazil))
      <|> (string "Canada"         >> return (Just Canada))
      <|> (string "England"        >> return (Just England))
      <|> (string "ENGLAND"        >> return (Just England))
      <|> (string "France"         >> return (Just France))
      <|> (string "FRANCE"         >> return (Just France))
      <|> (string "Finland"        >> return (Just Finland))
      <|> (string "Germany"        >> return (Just Germany))
      <|> (string "Hungary"        >> return (Just Hungary))
      <|> (string "Israel"         >> return (Just Israel))
      <|> (string "Italy"          >> return (Just Italy))
      <|> (string "Korea"          >> return (Just SouthKorea))
      <|> (string "Latvia"         >> return (Just Latvia))
      <|> (string "Netherlands"    >> return (Just Netherlands))
      <|> (string "New Zealand"    >> return (Just NewZealand))
      <|> (string "Norway"         >> return (Just Norway))
      <|> (string "Poland"         >> return (Just Poland))
      <|> (string "Russia"         >> return (Just Russia))
      <|> (string "Scotland"       >> return (Just Scotland))
      <|> (string "Singapore"      >> return (Just Singapore))
      <|> (string "SouthKorea"     >> return (Just SouthKorea))
      <|> (string "Spain"          >> return (Just Spain))
      <|> (string "Sweden"         >> return (Just Sweden))
      <|> (string "Switzerland"    >> return (Just Switzerland))
      <|> (string "Ukraine"        >> return (Just Ukraine))
      <|> (string "United Kingdom" >> return (Just UnitedKingdom))
      <|> (string "UK"             >> return (Just UnitedKingdom))
      <|> (string "United States"  >> return (Just UnitedStates))
      <|> return Nothing

      -- in case of the United States, we will only see the state...

state :: Parser (Maybe State)
state = (string "AL" >> return (Just Alabama))
    <|> (string "AK" >> return (Just Alaska))
    <|> (string "AZ" >> return (Just Arizona))
    <|> (string "AR" >> return (Just Arkansas))
    <|> (string "CA" >> return (Just California))
    <|> (string "CO" >> return (Just Colorado))
    <|> (string "CT" >> return (Just Connecticut))
    <|> (string "DC" >> return (Just DistrictOfColumbia))
    <|> (string "DE" >> return (Just Delaware))
    <|> (string "FL" >> return (Just Florida))
    <|> (string "GA" >> return (Just Georgia))
    <|> (string "HI" >> return (Just Hawaii))
    <|> (string "ID" >> return (Just Idaho))
    <|> (string "IL" >> return (Just Illinois))
    <|> (string "IN" >> return (Just Indiana))
    <|> (string "IA" >> return (Just Iowa))
    <|> (string "KS" >> return (Just Kansas))
    <|> (string "KY" >> return (Just Kentucky))
    <|> (string "LA" >> return (Just Louisiana))
    <|> (string "ME" >> return (Just Maine))
    <|> (string "MD" >> return (Just Maryland))
    <|> (string "MA" >> return (Just Massachusetts))
    <|> (string "MI" >> return (Just Michigan))
    <|> (string "MN" >> return (Just Minnesota))
    <|> (string "MS" >> return (Just Mississippi))
    <|> (string "MO" >> return (Just Montana))
    <|> (string "NE" >> return (Just Nebraska))
    <|> (string "NV" >> return (Just Nevada))
    <|> (string "NH" >> return (Just NewHampshire))
    <|> (string "NJ" >> return (Just NewJersey))
    <|> (string "NM" >> return (Just NewMexico))
    <|> (string "NY" >> return (Just NewYork))
    <|> (string "NC" >> return (Just NorthCarolina))
    <|> (string "ND" >> return (Just NorthDakota))
    <|> (string "OH" >> return (Just Ohio))
    <|> (string "OK" >> return (Just Oklahoma))
    <|> (string "OR" >> return (Just Oregon))
    <|> (string "PA" >> return (Just Pennsylvania))
    <|> (string "RI" >> return (Just RhodeIsland))
    <|> (string "SC" >> return (Just SouthCarolina))
    <|> (string "SD" >> return (Just SouthDakota))
    <|> (string "TN" >> return (Just Tennessee))
    <|> (string "TX" >> return (Just Texas))
    <|> (string "UT" >> return (Just Utah))
    <|> (string "VT" >> return (Just Vermont))
    <|> (string "VA" >> return (Just Virginia))
    <|> (string "WA" >> return (Just Washington))
    <|> (string "WV" >> return (Just WestVirginia))
    <|> (string "WI" >> return (Just Wisconsin))
    <|> (string "WY" >> return (Just Wyoming))

    -- states in Canada
    <|> (string "AB" >> return (Just Alberta))
    <|> (string "BC" >> return (Just BritishColumbia))
    <|> (string "NB" >> return (Just NewBrunswick))
    <|> (string "NL" >> return (Just Newfoundland))
    <|> (string "NS" >> return (Just NovaScotia))
    <|> (string "MB" >> return (Just Manitoba))
    <|> (string "ON" >> return (Just Ontario))
    <|> (string "PE" >> return (Just PrinceEdwardIsland))
    <|> (string "QC" >> return (Just Quebec))
    <|> (string "SK" >> return (Just Saskatchewan))

    <|> return Nothing


city :: Parser (Maybe City)
city = do parsedCity <- takeWhile1 (/= ',')
          return . Just . City $ parsedCity
