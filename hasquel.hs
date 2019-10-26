import Control.Monad (forever, when)
import System.IO (isEOF)
import System.Exit (exitSuccess)

-- This is reading a GeoNames file, here is what is expected:
-- The main 'geoname' table has the following fields :
-- ---------------------------------------------------
-- geonameid         : integer id of record in geonames database
-- name              : name of geographical point (utf8) varchar(200)
-- asciiname         : name of geographical point in plain ascii characters, varchar(200)
-- alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
-- latitude          : latitude in decimal degrees (wgs84)
-- longitude         : longitude in decimal degrees (wgs84)
-- feature class     : see http://www.geonames.org/export/codes.html, char(1)
-- feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
-- country code      : ISO-3166 2-letter country code, 2 characters
-- cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
-- admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
-- admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
-- admin3 code       : code for third level administrative division, varchar(20)
-- admin4 code       : code for fourth level administrative division, varchar(20)
-- population        : bigint (8 byte int) 
-- elevation         : in meters, integer
-- dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
-- timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
-- modification date : date of last modification in yyyy-MM-dd format


data Place = Place { geonameid :: Int
  , name :: String
  , asciiname :: String
  , alternate :: String
  , latitude :: Double
  , longitude :: Double
  , featureClass :: String
  , featureCode :: String
  , countryCode :: String
  , cc2 :: String
  , admin1 :: String
  , admin2 :: String
  , admin3 :: String
  , admin4 :: String
  , population :: Int
  , elevation :: String
  , dem :: Int
  , timezone :: String
  , lastModified :: String
  } deriving (Show)

-- | Parse GeoNames records
-- This function will take a tab separated line (String) and return a Place object.
--
-- $setup
-- >>> let inputStr = "2130833\tMcArthur Reef\tMcArthur Reef\t\t52.06667\t177.86667\tU\tRFU\tUS\t\tAK\t016\t\t\t0\t\t-9999\tAsia/Kamchatka\t2016-07-05"
--
-- Examples:
-- >>> makePlace inputStr
-- Place {geonameid = 2130833, name = "McArthur Reef", asciiname = "McArthur Reef", alternate = "", latitude = 52.06667, longitude = 177.86667, featureClass = "U", featureCode = "RFU", countryCode = "US", cc2 = "", admin1 = "AK", admin2 = "016", admin3 = "", admin4 = "", population = 0, elevation = "", dem = -9999, timezone = "Asia/Kamchatka", lastModified = "2016-07-05"} 
--
-- FIXME: This code has way too much repetition, could it be possible to annotate the fields on the Place definition with their order in the csv line and use it's type definition to not repeat any variable name below?
makePlace :: String -> Place
makePlace line = do
    let yy = split line
    -- this is only meant to work for GeoNames CSV and those always have all the fields
    -- FIXME: assert length yy \= 19 
    let [geonameid, name, asciiname, alternate, latitude, longitude, featureClass, featureCode, countryCode, cc2, admin1, admin2, admin3, admin4, population, elevation, dem, timezone, lastModified]  = yy
    let p = Place (read geonameid :: Int) name asciiname alternate (read latitude :: Double) (read longitude :: Double) featureClass featureCode countryCode cc2 admin1 admin2 admin3 admin4 (read population :: Int) elevation (read dem :: Int) timezone lastModified
    p


-- | Split lines on tab characters
-- Examples:
-- >>> split "hola\tmundo"
-- ["hola","mundo"]
-- 
split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == '\t' = "": rest
   | otherwise = (c: head rest) : tail rest
  where
    rest = split cs


main = parsero

parsero = forever $ do
    done <- isEOF
    when done $  exitSuccess
    z <- getLine
    let p = makePlace z
    putStrLn (show p)
    parsero
