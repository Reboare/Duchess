{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
module Base.Types where

import qualified Data.Text as T
import Data.List
import Base.Magic

type Url = String

data Media = Movie [MediaType]
            |Episode [MediaType]
            deriving (Eq, Show)

class ToMediaType a where
    toMedia :: a -> Media

class UniqType a where
    dup :: a -> a -> Bool

-- Really Want to automate this to use Template Haskell
-- At Some Point.  Don't really know TH that well yet
-- I'm well aware this design is terrible 
--A single ADT would probably simplify so much of this code, 
--but I'm still unsure of what exactly constitutes a MediaType yet
--When I am I'll freeze it and merge all of this into two ADT's for
--Episode and Movie 
data MediaType =  Title T.Text
                | Year Int
                | Resolution ResolutionDer
                | Source SourceDer
                | Codec CodecDer
                | Part Int
                | Runtime Int
                | IMDBid Int
                | IMDBRating Float
                | Synopsis T.Text
                | Actors [Actor]
                | EpisodeNo Int 
                | SeasonNo Int
                | Poster (Either Url FilePath) -- On the left, we need to fetch it.  On the right, it's been fetched.
                deriving (Eq, Show, Ord)

instance UniqType MediaType where
    dup (Title _) (Title _) = True
    dup (Title _) _ = False
    dup (Year _) (Year _) = True
    dup (Year _) _ = False
    dup (Resolution _) (Resolution _) = True
    dup (Resolution _) _ = False
    dup (Source _) (Source _) = True
    dup (Source _) _ = False
    dup (Codec _) (Codec _) = True
    dup (Codec _) _ = False
    dup (Part _) (Part _) = True
    dup (Part _) _ = False
    dup (Runtime _) (Runtime _) = True
    dup (Runtime _) _ = False
    dup (IMDBid _) (IMDBid _) = True
    dup (IMDBid _) _ = False
    dup (IMDBRating _) (IMDBRating _) = True
    dup (IMDBRating _) _ = False
    dup (Synopsis _) (Synopsis _) = True
    dup (Synopsis _) _ = False
    dup (Actors _) (Actors _) = True
    dup (Actors _) _ = False
    dup (EpisodeNo _) (EpisodeNo _) = True
    dup (EpisodeNo _) _ = False
    dup (SeasonNo _) (SeasonNo _) = True
    dup (SeasonNo _) _ = False
    dup (Poster _) (Poster _) = True
    dup (Poster _) _ = False


merge :: Media -> Media -> Media
-- |Merges two Movies or episodes where the first argument has priority in all conflicts
-- |e.g. if both mHead and mTail have a Title then mHead's title will be the only one to survive
-- This may have to be done with Template Haskell
merge (Movie mHead) (Movie mTail) = Movie $! mHead ++ (rec mHead mTail)
    where 
        rec [] tailEn = tailEn
        rec (x:xs) tailEn = rec xs (filter (\z -> not (dup z x)) tailEn) 
merge (Episode mHead) (Episode mTail) = Episode $! mHead ++ (rec mHead mTail)
    where 
        rec [] tailEn = tailEn
        rec (x:xs) tailEn = rec xs (filter (\z -> not (dup z x)) tailEn)
merge _ _ = error "Unmergable data types"

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

--Did this to cut down on the amount of boilerplate I was writing
--Also learned a bit of TH while doing it so yay
$(genFind 'Title "title")
$(genFind 'Year "year")
$(genFind 'Resolution "res")
$(genFind 'Source "source")
$(genFind 'Codec "codec")
$(genFind 'Part "part")
$(genFind 'Runtime "runtime")
$(genFind 'IMDBid "imdbID")
$(genFind 'IMDBRating "imdbRating")
$(genFind 'Synopsis "synopsis")
$(genFind 'Actors "actors")
$(genFind 'EpisodeNo "episode")
$(genFind 'SeasonNo "season")
$(genFind 'Poster "poster")