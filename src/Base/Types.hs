{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Base.Types where

import           Control.Lens
import           Control.Monad.Maybe
import           Data.List
import qualified Data.Text           as T

type Url = String
type MaybeIO = MaybeT IO 


data MediaFile = MediaFile {
    _filePath   :: String,
    _info       :: NMedia,
    _resolution :: Maybe ResolutionDer,
    _src        :: Maybe SourceDer,
    _cdc        :: Maybe CodecDer,
    _part       :: Maybe Int
}   deriving (Eq, Show, Ord)

data NMedia = NMedia {
    _title      :: T.Text,
    _year       :: Maybe Int,
    _runtime    :: Maybe Int,
    _imdbid     :: Maybe Int,
    _imdbrating :: Maybe Float,
    _synopsis   :: Maybe T.Text,
    _actors     :: Maybe [Actor],
    _episodeno  :: Maybe Int,
    _seriesno   :: Maybe Int,
    _poster     :: Maybe (Either Url FilePath)
}   deriving (Eq, Show, Ord)



data Actor = Actor T.Text
            deriving (Show, Eq, Ord)



data SourceDer = DVD | BD | HDDVD | VHS | WEBDL | HDTV | PRE
                deriving (Eq, Show, Ord)

data CodecDer = H264 | VC1 | XVID
                deriving (Eq, Show, Ord)

data ResolutionDer = ResolutionDer Int Signal
            deriving (Eq, Ord)

instance Show ResolutionDer where
    show (ResolutionDer x s) = show x ++ show s


data Signal = Progressive | Interlaced
            deriving (Eq, Ord)

instance Show Signal where
    show Progressive = "p"
    show Interlaced = "i"

strToSignal :: Char -> Signal
strToSignal 'i' = Interlaced
strToSignal 'p' = Progressive


$(makeLenses ''NMedia)
$(makeLenses ''MediaFile)