{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
module Base.Types where

import qualified Data.Text as T
import Data.List

data Media = Movie [MediaType]
            |Episode [MediaType]
            deriving (Eq, Show)

-- Really Want to automate this to use Template Haskell
-- At Some Point.  Don't really know TH that well yet
data MediaType =  Title T.Text
                | Year Int
                | Resolution ResolutionDer
                | Source SourceDer
                | Codec CodecDer
                | Part Int
                | IMDBid Int
                | Synopsis T.Text
                | Actors [Actor]
                | EpisodeNo Int -- FOr later.  Will rename MovieType to the more generic MediaType
                | SeasonNo Int
                deriving (Eq, Show, Ord)

title xs = find eqTitle xs
    where
        eqTitle (Title a) = True
        eqTitle _ = False

year xs = find eqYear xs
    where
        eqYear (Year a) = True
        eqYear _ = False

res xs = find eqRes xs
    where
        eqRes (Resolution a) = True
        eqRes _ = False

source xs = find eqSrc xs
    where
        eqSrc (Source a) = True
        eqSrc _ = False

codec xs = find eqCdc xs
    where
        eqCdc (Codec a) = True
        eqCdc _ = False

part xs = find eqPart xs
    where
        eqPart (Part a) = True
        eqPart _ = False

imdb xs = find eqIMDBid xs
    where
        eqIMDBid (IMDBid a) = True
        eqIMDBid _ = False

synopsis xs = find eqSynopsis xs
    where
        eqSynopsis (Synopsis a) = True
        eqSynopsis _ = False

actors xs = find eqActors xs
    where
        eqActors (Actors a) = True
        eqActors _ = False

episode xs = find eqEpisode xs
    where
        eqEpisode (EpisodeNo a) = True
        eqEpisode _ = False

season xs = find eqSeason xs
    where
        eqSeason (SeasonNo a) = True
        eqSeason _ = False

data Actor = Actor T.Text 
            deriving (Show, Eq, Ord)

data File = File {
                    path :: FilePath,
                    partNo :: Int   
                 }
            deriving (Eq, Show)


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
