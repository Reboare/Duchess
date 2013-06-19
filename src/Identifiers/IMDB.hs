{-# LANGUAGE OverloadedStrings #-}
module Identifiers.IMDB
(
    search,
    sResultUpdateNMedia,
    tResultUpdateNMedia
)
where

import           Base.HTTP
import           Base.Types
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Data
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Typeable

data TitleResult = TitleResult {
                ttitle      :: T.Text, --Movie Title
                tyear       :: T.Text, --Year Movie was Released
                trated      :: T.Text, --The Age Rating
                treleased   :: T.Text, --The Full Release Date
                truntime    :: T.Text,
                tgenre      :: T.Text,
                tdirector   :: T.Text,
                twriter     :: T.Text,
                tactors     :: T.Text,
                tplot       :: T.Text,
                tposter     :: T.Text, --Url to Poster
                timdbRating :: T.Text,
                timdbVotes  :: T.Text,
                timdbID     :: T.Text,
                toType      :: T.Text,
                tresponse   :: T.Text}
                deriving (Eq, Show)

data SearchResult = SearchResult {
                sTitle  :: T.Text,
                sYear   :: T.Text,
                sImdbID :: T.Text,
                sOType  :: T.Text}
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

search :: T.Text -> Maybe Int -> MaybeIO [SearchResult]
search title year = MaybeT $! do
    let y = case year of
                Just a -> "&y=" ++ show a
                Nothing -> ""
    let t = T.unpack $! T.intercalate "%20" (T.splitOn " " title)
    doc <- getHTML $! "http://www.omdbapi.com/?s=" ++ t ++ y
    return  (unSObject <$> (decode doc :: Maybe SearchObject))


sResultUpdateNMedia :: NMedia -> SearchResult -> NMedia
sResultUpdateNMedia nMedia sres =
    (setImdb (sImdbID sres)) $!
    (setYear (sYear sres)) $!
    (setTitle (sTitle sres) nMedia)
    where
        setTitle = set title
        setYear val = set year (Just (read (T.unpack val) :: Int))
        setImdb val = set imdbid (Just (read (T.unpack val) :: Int))

tResultUpdateNMedia :: NMedia -> TitleResult -> NMedia
tResultUpdateNMedia = undefined