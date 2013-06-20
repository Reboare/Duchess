{-# LANGUAGE OverloadedStrings #-}
module Identifiers.IMDB
(
    search,
    getTitleInfo,
    updateInfo,
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
import           Data.Attoparsec.Text
import           Control.Monad.Maybe

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
        setImdb val = set imdbid (Just $! T.unpack val)



getTitleInfo :: T.Text -> Maybe Int -> Maybe String -> MaybeIO TitleResult
getTitleInfo theTitle theYear theIMDBid = MaybeT  $! do
    let y = case theYear of
                Just a -> "&y="++(show a)
                Nothing -> ""
    let t = T.unpack $! T.intercalate "%20" (T.splitOn " " theTitle)
    let idb = case theIMDBid of
                Just a -> "&i="++a
                Nothing -> ""
    doc <- getHTML $! "http://www.omdbapi.com/?t=" ++ t ++ y ++ idb
    return  (decode doc :: Maybe TitleResult)



tResultUpdateNMedia :: NMedia -> TitleResult -> NMedia
-- |Still unfinished.  Need to add actors, episode, series and poster
tResultUpdateNMedia nmedia tres = 
    set title (ttitle tres) $! --Add string diff calc to this
    uIntIfNothing (view year nmedia) year (tyear tres) $
        uNothingNoJust (view runtime nmedia) runtime (getRuntime $! truntime tres) $
            uIfNothing (view imdbid nmedia) imdbid (T.unpack $! timdbID tres) $
                uFloatIfNothing (view imdbrating nmedia) imdbrating (timdbRating tres) $
                    uIfNothing (view synopsis nmedia) synopsis (tplot tres) $
                        uIfNothing (view actors nmedia) actors (unpackActors (tactors tres)) nmedia

    where
        unpackActors :: T.Text -> [Actor]
        unpackActors tactors = fmap (Actor) $! T.splitOn ", " tactors
        uNothingNoJust (Just a) field val nmed = nmed 
        uNothingNoJust Nothing field val nmed = set field (val) nmed
        uIfNothing  (Just a) field val nmed = nmed
        uIfNothing  Nothing field val nmed = set field (Just val) nmed 
        uIntIfNothing  (Just a) field val nmed = nmed
        uIntIfNothing  Nothing field val nmed = set field (Just (read (T.unpack val) :: Int)) nmed 
        uFloatIfNothing  (Just a) field val nmed = nmed
        uFloatIfNothing  Nothing field val nmed = set field (Just (read(T.unpack val) :: Float)) nmed 


updateInfo :: MediaFile -> IO MediaFile
updateInfo mfile = do
    gotteninfo <- runMaybeT $! getTitleInfo (view (info.title) mfile) (view (info.year) mfile) (view (info.imdbid) mfile)
    return $! case gotteninfo of
        Nothing -> mfile
        (Just ginfo) -> set info (tResultUpdateNMedia (view info mfile) ginfo) mfile

parserRuntime :: Parser Int
parserRuntime = do
    x <- many1 (choice [hour, minute])
    return $! sum x
    where 
        hour = do
            x <- double 
            " h "
            return $! (floor x) * 60
        minute = do
            y <- double
            " min"
            return $! floor y

getRuntime :: T.Text -> Maybe Int
getRuntime txt = maybeResult $! feed (parse parserRuntime txt) T.empty

_baseMedia :: NMedia
_baseMedia = NMedia "Grandma's Boy" (Just 2006) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
_baseFile :: MediaFile
_baseFile = MediaFile "" _baseMedia Nothing Nothing Nothing Nothing
    
main2 = print =<< runMaybeT (getTitleInfo "vincent" Nothing Nothing)
main1 = print =<< updateInfo _baseFile
