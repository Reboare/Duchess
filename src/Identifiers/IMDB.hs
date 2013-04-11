{-#LANGUAGE OverloadedStrings#-}
module Identifiers.IMDB 
(
    search,
    fromSearch,
    getTitle
)
where
import           Base.HTTP
import           Data.Aeson
import Base.Types (ToMediaType(..), Media(..), MediaType(..), Actor(..))
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Data
import Data.Typeable
import Data.List.Split



data TitleResult = TitleResult {
                title :: T.Text, --Movie Title
                year :: T.Text, --Year Movie was Released
                rated :: T.Text, --The Age Rating
                released :: T.Text, --The Full Release Date
                runtime :: T.Text, 
                genre :: T.Text,
                director :: T.Text,
                writer :: T.Text,
                actors :: T.Text,
                plot :: T.Text,
                poster :: T.Text, --Url to Poster
                imdbRating :: T.Text,
                imdbVotes :: T.Text,
                imdbID :: T.Text,
                oType :: T.Text,
                response :: T.Text}
                deriving (Eq, Show)

instance ToMediaType TitleResult  where
    toMedia xs = constr [cTitle, cYear,cActors, cPlot, cID, cRat]
        where
            cTitle = Title $! title xs
            cYear = Year $! (read (T.unpack $! year xs):: Int)
            cRuntime = Runtime $! (read (T.unpack $! runtime xs):: Int)
            cActors = Actors $! map Actor $! map (T.strip) (T.splitOn "," $! actors xs)
            cPlot = Synopsis $! plot xs
            cID = IMDBid $! (read (drop 2 $! T.unpack $! imdbID xs):: Int)
            cRat = IMDBRating $! (read (T.unpack $! imdbRating xs):: Float)
            constr = case oType xs of
                        "movie" -> Movie
                        "episode" -> Episode 
                        "series" -> Episode


data SearchResult = SearchResult {
                sTitle :: T.Text,
                sYear :: T.Text,
                sImdbID :: T.Text,
                sOType :: T.Text} 
                deriving (Eq, Show)

newtype SearchObject = SearchObject {unSObject :: [SearchResult]} deriving (Eq, Show)

instance FromJSON SearchObject where
    parseJSON (Object v) = SearchObject <$>
                            v .: "Search" 
    parseJSON _          = mzero

instance FromJSON TitleResult where
    parseJSON (Object v) = TitleResult <$>
                            v .: "Title" <*> v .: "Year" <*> v .: "Rated" <*>
                            v .: "Released" <*> v .: "Runtime" <*> v .: "Genre" <*>
                            v .: "Director" <*> v .: "Writer" <*> v .: "Actors" <*>
                            v .: "Plot" <*> v .: "Poster" <*> v .: "imdbRating" <*>
                            v .: "imdbVotes" <*> v .: "imdbID" <*> v .: "Type" <*>
                            v .: "Response"
    parseJSON _          = mzero


instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult <$>
                            v .: "Title" <*>
                            v .: "Year" <*>
                            v .: "imdbID" <*>
                            v .: "Type"
    parseJSON _          = mzero

search :: T.Text -> Maybe Int -> IO (Maybe [SearchResult])
search title year = do
    let y = case year of
                Just a -> "&y="++(show a)
                Nothing -> ""
    let t = T.unpack $! T.intercalate "%20" (T.splitOn " " title)
    doc <- getHTML $! "http://www.omdbapi.com/?s=" ++ t ++ y
    return $! unSObject <$> (decode doc :: Maybe SearchObject)

fromSearch :: SearchResult -> IO (Maybe Media)
fromSearch (SearchResult ttl yr idb _) = getTitle ttl (Just $!(read (T.unpack yr) :: Int)) (Just idb)

getTitle :: T.Text -> Maybe Int -> Maybe T.Text -> IO (Maybe Media)
getTitle theTitle theYear theIMDBid = do
    let y = case theYear of
                Just a -> "&y="++(show a)
                Nothing -> ""
    let t = T.unpack $! T.intercalate "%20" (T.splitOn " " theTitle)
    let idb = case theIMDBid of
                Just a -> "&i="++(show a)
                Nothing -> ""
    doc <- getHTML $! "http://www.omdbapi.com/?t=" ++ t ++ y ++ idb
    return $! toMedia <$> (decode doc :: Maybe TitleResult)