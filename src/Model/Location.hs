module Model.Location
  where

import ClassyPrelude
import Data.Attoparsec.Text

data Location = Location { locationText    :: !Text
                         , locationCity    :: !(Maybe City)
                         , locationState   :: !(Maybe State)
                         , locationCountry :: !(Maybe Country)
                         }
  deriving (Eq, Ord, Show)

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

             -- some others that appear...
           | UnitedKingdom'     -- UK
  deriving (Eq, Ord, Show)


parseLocation :: Text -> Location
parseLocation t = case parseOnly location t of
  Left  _   -> Location t Nothing Nothing Nothing
  Right loc -> loc

location :: Parser Location
location = (do skipSpace
               parsedCountry <- country
               skipSpace
               return parsedCountry
           )
       <|> (do skipSpace
               parsedCity <- city
               skipSpace
               char ","
               skipSpace
               parsedCountry <- country
               skipSpace
           )

country :: Parser Country
country = (string "Australia"      >> return Australia)
      <|> (string "Austria"        >> return Austria)
      <|> (string "Brazil"         >> return Brazil)
      <|> (string "Canada"         >> return Canada)
      <|> (string "England"        >> return England)
      <|> (string "France"         >> return France)
      <|> (string "Finland"        >> return Finland)
      <|> (string "Germany"        >> return Germany)
      <|> (string "Hungary"        >> return Hungary)
      <|> (string "Israel"         >> return Israel)
      <|> (string "Italy"          >> return Italy)
      <|> (string "Korea"          >> return SouthKorea)
      <|> (string "Latvia"         >> return Latvia)
      <|> (string "Netherlands"    >> return Netherlands)
      <|> (string "New Zealand"    >> return NewZealand)
      <|> (string "Norway"         >> return Norway)
      <|> (string "Poland"         >> return Poland)
      <|> (string "Russia"         >> return Russia)
      <|> (string "Scotland"       >> return Scotland)
      <|> (string "Singapore"      >> return Singapore)
      <|> (string "SouthKorea"     >> return SouthKorea)
      <|> (string "Spain"          >> return Spain)
      <|> (string "Sweden"         >> return Sweden)
      <|> (string "Switzerland"    >> return Switzerland)
      <|> (string "Ukraine"        >> return Ukraine)
      <|> (string "United Kingdom" >> return UnitedKingdom)
      <|> (string "UK"             >> return UnitedKingdom)
      <|> (string "United States"  >> return UnitedStates)

      -- in case of the United States, we will only see the state...

state :: Parser State
state = (string "AL" >> return Alabama)
    <|> (string "AK" >> return Alaska)
    <|> (string "AZ" >> return Arizona)
    <|> (string "AR" >> return Arkansas)
    <|> (string "CA" >> return California)
    <|> (string "CO" >> return Colorado)
    <|> (string "CT" >> return Connecticut)
    <|> (string "DC" >> return DistrictOfColumbia)
    <|> (string "DE" >> return Delaware)
    <|> (string "FL" >> return Florida)
    <|> (string "GA" >> return Georgia)
    <|> (string "HI" >> return Hawaii)
    <|> (string "ID" >> return Idaho)
    <|> (string "IL" >> return Illinois)
    <|> (string "IN" >> return Indiana)
    <|> (string "IA" >> return Iowa)
    <|> (string "KS" >> return Kansas)
    <|> (string "KY" >> return Kentucky)
    <|> (string "LA" >> return Louisiana)
    <|> (string "ME" >> return Maine)
    <|> (string "MD" >> return Maryland)
    <|> (string "MA" >> return Massachusetts)
    <|> (string "MI" >> return Michigan)
    <|> (string "MN" >> return Minnesota)
    <|> (string "MS" >> return Mississippi)
    <|> (string "MO" >> return Montana)
    <|> (string "NE" >> return Nebraska)
    <|> (string "NV" >> return Nevada)
    <|> (string "NH" >> return NewHampshire)
    <|> (string "NJ" >> return NewJersey)
    <|> (string "NM" >> return NewMexico)
    <|> (string "NY" >> return NewYork)
    <|> (string "NC" >> return NorthCarolina)
    <|> (string "ND" >> return NorthDakota)
    <|> (string "OH" >> return Ohio)
    <|> (string "OK" >> return Oklahoma)
    <|> (string "OR" >> return Oregon)
    <|> (string "PA" >> return Pennsylvania)
    <|> (string "RI" >> return RhodeIsland)
    <|> (string "SC" >> return SouthCarolina)
    <|> (string "SD" >> return SouthDakota)
    <|> (string "TN" >> return Tennessee)
    <|> (string "TX" >> return Texas)
    <|> (string "UT" >> return Utah)
    <|> (string "VT" >> return Vermont)
    <|> (string "VA" >> return Virginia)
    <|> (string "WA" >> return Washington)
    <|> (string "WV" >> return WestVirginia)
    <|> (string "WI" >> return Wisconsin)
    <|> (string "WY" >> return Wyoming)

    -- states in Canada
    <|> (string "AB" >> return Alberta)
    <|> (string "BC" >> return BritishColumbia)
    <|> (string "NB" >> return NewBrunswick)
    <|> (string "NL" >> return Newfoundland)
    <|> (string "NS" >> return NovaScotia)
    <|> (string "MB" >> return Manitoba)
    <|> (string "ON" >> return Ontario)
    <|> (string "PE" >> return PrinceEdwardIsland)
    <|> (string "QC" >> return Quebec)
    <|> (string "SK" >> return Saskatchewan)
