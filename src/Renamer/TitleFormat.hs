{-# LANGUAGE OverloadedStrings #-}
module Renamer.TitleFormat
(genRenamer')

where
import           Base.Types
import           Control.Lens
import           Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text       as AT
import qualified Data.Text                  as T
import qualified System.Directory           as D
import           System.FilePath
import Debug.Trace
import Control.Monad
import Control.Applicative

data CheckCall = LeftC T.Text | RightC T.Text | FillerC T.Text deriving (Eq, Show)
data Renamer = Renamer String deriving (Eq, Show)

baseMedia :: NMedia
baseMedia = NMedia "Grandma's Boy" (Just 2006) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
baseFile :: MediaFile
baseFile = MediaFile "" baseMedia Nothing Nothing Nothing Nothing

genRenamer' :: T.Text -> MediaFile -> T.Text
genRenamer' fmt medf = case (AT.maybeResult cCallList ) of
    (Just a) -> T.strip.T.concat $! dropleft a
    Nothing -> T.pack (view filePath medf)
    where
        cCallList = AT.feed (AT.parse (many1 (AT.choice (choices medf))) fmt) T.empty


choices medf = fany:(map (\x -> x medf) 
    [ftitle, fyear, fruntime, frating, fimdbid, fepisode, fseries, fpart, fenclosure])

dropleft (x:xs) = case x of
            (LeftC a) -> (dropleft xs)
            (RightC a) -> a:(dropleft xs)
            (FillerC a) -> a:(dropleft xs)
dropleft [] = [] 

fenclosure :: MediaFile -> AT.Parser CheckCall
fenclosure mfile = do
-- |If no data enclosed within this returns true then through anything inside away
    AT.string "["
    x <- AT.many' $ choice (choices mfile)
    if sthrough x 
        then return $! RightC $! (T.concat.dropleft) x
        else return $! LeftC $! (T.concat.dropleft) x
    where
        sthrough ((RightC a):xs) = True
        sthrough ((LeftC a):xs) = sthrough xs
        sthrough ((FillerC a):xs) = sthrough xs
        sthrough [] = False 
        

fany :: AT.Parser CheckCall
fany = do
    x <- AT.takeWhile1 (\xs -> not (elem xs "[]$"))
    return (FillerC x)

ftitle :: MediaFile -> AT.Parser CheckCall
ftitle mfile = do
    x <- AT.string "$title"
    return $! RightC $! view (info.title) mfile

fyear :: MediaFile -> AT.Parser CheckCall
fyear mfile = do
    AT.string "$year"
    return $! case view (info.year) mfile of
        (Just a) -> RightC $! T.pack $! show a
        Nothing -> LeftC T.empty

fruntime :: MediaFile -> AT.Parser CheckCall
fruntime mfile = do
    AT.string "$len"
    return $! case view (info.runtime) mfile of
        Just a -> RightC $! T.pack $! show a
        Nothing -> LeftC T.empty

frating :: MediaFile -> AT.Parser CheckCall
frating mfile = do
    AT.string "$irat"
    return $! case view (info.imdbrating) mfile of
        Just a -> RightC $! T.pack $! show a
        Nothing -> LeftC T.empty

fimdbid :: MediaFile -> AT.Parser CheckCall
fimdbid mfile = do
    AT.string "$id"
    return $! case view (info.imdbid) mfile of
        Just a -> RightC $! T.pack $! show a
        Nothing -> LeftC T.empty

fepisode :: MediaFile -> AT.Parser CheckCall
fepisode mfile = do
    AT.string "$ep"
    return $! case view (info.episodeno) mfile of
        Just a -> RightC $! T.pack $! show a
        Nothing -> LeftC T.empty

fseries :: MediaFile -> AT.Parser CheckCall
fseries mfile = do
    AT.string "$series"
    return $! case view (info.seriesno) mfile of
        Just a -> RightC $! T.pack $! show a
        Nothing -> LeftC $! T.empty

fpart :: MediaFile -> AT.Parser CheckCall
fpart mfile = do
    AT.string "$part"
    return $! case view part mfile of
        Just a -> RightC $! T.pack $! show a 
        Nothing -> LeftC T.empty