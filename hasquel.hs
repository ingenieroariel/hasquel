import Control.Monad (forever, when)
import System.IO (isEOF)
import System.Exit (exitSuccess)
import System.Environment (getArgs)
import Data.List (transpose)

-- This is reading a GeoNames file, here is what is expected:
-- The main 'geoname' table has the following fields :
-- ---------------------------------------------------
-- 0 geonameid         : integer id of record in geonames database
-- 1 name              : name of geographical point (utf8) varchar(200)
-- 2 asciiname         : name of geographical point in plain ascii characters, varchar(200)
-- 3 alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
-- 4 latitude          : latitude in decimal degrees (wgs84)
-- 5 longitude         : longitude in decimal degrees (wgs84)
-- 6 feature class     : see http://www.geonames.org/export/codes.html, char(1)
-- 7 feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
-- 8 country code      : ISO-3166 2-letter country code, 2 characters
-- 9 cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
-- 10 admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
-- 11 admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
-- 12 admin3 code       : code for third level administrative division, varchar(20)
-- 13 admin4 code       : code for fourth level administrative division, varchar(20)
-- 14 population        : bigint (8 byte int) 
-- 15 elevation         : in meters, integer
-- 16 dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
-- 17 timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
-- 18 modification date : date of last modification in yyyy-MM-dd format

-- | Parse GeoNames records
-- This function will take a tab separated line (String) and return a Matrix object
--
-- $setup
-- >>> let inputStr = "2130833\tMcArthur Reef\tMcArthur Reef\t\t52.06667\t177.86667\tU\tRFU\tUS\t\tAK\t016\t\t\t0\t\t-9999\tAsia/Kamchatka\t2016-07-05"
--
-- Examples:
-- >>> makeMatrix inputStr
-- []
--

-- geonameid, latitude, longitude, dem
parsePlace :: String -> [Double]
parsePlace line = let yy = (split line) in [
                     (read (yy !! 0) :: Double) ,
                     (read (yy !! 4) :: Double),
                     (read (yy !! 5) :: Double) ,
                     (read (yy !! 14) :: Double),
                     (read (yy !! 16) :: Double)]


-- sampleSingleInputStr = "2130833\tMcArthur Reef\tMcArthur Reef\t\t52.06667\t177.86667\tU\tRFU\tUS\t\tAK\t016\t\t\t0\t\t-9999\tAsia/Kamchatka\t2016-07-05"
-- single = foldr (<->) (zero 1 5) [ makeMatrix x | x <- (take 2000 (repeat sampleSingleInputStr ))]


parsePlaces :: [String] -> [[Double]]
parsePlaces l = [parsePlace x | x <- l ]

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


-- dumb distance calculation, not taking geoid into account.
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2

-- List of distances
distanceM :: [[Double]] -> (Double, Double) -> [Double]
distanceM m (lat, lon) = [(distance (lat, lon) (x !! 1, x !! 2)) | x <- m] 

-- annotate a matrix with distance to a point
withDistanceTo :: [[Double]] -> (Double, Double) -> [[Double]]
withDistanceTo m (lat, lon) = transpose (m' ++ d')
   where
     m' = transpose m
     d' = [distanceM m (lat, lon)]


-- | main
-- Cool
main :: IO()
main = do
    args <- getArgs 
    -- FIXME: Assert len of args is 3, the expected order is:
    -- 0           1     2    
    -- fileName  <lat> <lon>
    f <- readFile ( args !! 0)
    let m = parsePlaces (lines f)

    -- add distance column
    let lat = read ( args !! 1) :: Double
    let lon = read ( args !! 2) :: Double

    let md = withDistanceTo m (lat, lon)
    
    putStrLn (show md)
