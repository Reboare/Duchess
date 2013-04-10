{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
module Base.Types where

import qualified Data.Text as T
import Data.List
import Language.Haskell.TH.Syntax
import Language.Haskell.TH

data Media = Movie [MediaType]
            |Episode [MediaType]
            deriving (Eq, Show)


eqTitle (Title a) = True
eqTitle _ = False

eqYear (Year a) = True
eqYear _ = False

eqRes (Resolution a) = True
eqRes _ = False

eqSrc (Source a) = True
eqSrc _ = False

eqCdc (Codec a) = True
eqCdc _ = False

eqPart (Part a) = True
eqPart _ = False

eqIMDBid (IMDBid a) = True
eqIMDBid _ = False

eqSynopsis (Synopsis a) = True
eqSynopsis _ = False

eqActors (Actors a) = True
eqActors _ = False

eqEpisode (EpisodeNo a) = True
eqEpisode _ = False

eqSeason (SeasonNo a) = True
eqSeason _ = False

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
year xs = find eqYear xs
res xs = find eqRes xs
source xs = find eqSrc xs
codec xs = find eqCdc xs
part xs = find eqPart xs
imdb xs = find eqIMDBid xs
synopsis xs = find eqSynopsis xs
actors xs = find eqActors xs
episode xs = find eqEpisode xs
season xs = find eqSeason xs

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
