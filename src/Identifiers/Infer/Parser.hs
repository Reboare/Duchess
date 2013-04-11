{-#LANGUAGE OverloadedStrings#-}
module Identifiers.Infer.Parser 
(
    mainParse
)
where

import Base.Types (MediaType(..), SourceDer(..), CodecDer(..), ResolutionDer(..), strToSignal)
import Data.Attoparsec.Text
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List

--TODO
--Add a filter for attributes of the form "[attr]"
--They're unstandardised in pretty much anny community so best job would be
--a preprocessor that chucks em out the window
attribute :: Parser ()
attribute = do
    char '['
    res <- takeWhile1 (']' /=)
    char ']'
    return ()

seperators :: [Char]
seperators = ". _[]"

year :: Parser MediaType
year =  do
    year <- count 4 digit
    return (Year $! (read year :: Int))



source :: Parser MediaType
source = Source <$> convertToSrc <$> (choice $ map asciiCI ["r5", "bluray","bdrip", "BRRip", 
                                                            "HDDVD", "HDTV", "DVDRip", 
                                                            "VHS", "Screener",  "WEB-DL", "NTSC"])

codec :: Parser MediaType
codec = Codec <$> convertToCdc <$> (choice $ map asciiCI ["xvid", "x264", "h264", "divx", 
                                         "AVC", "VC-1"])

resolution :: Parser MediaType
resolution = do
            res <- T.pack <$> (many1 digit)
            inc <- (char 'p' <|> char 'i')
            return $! Resolution $! ResolutionDer (read (T.unpack res) :: Int) (strToSignal inc)

part :: Parser MediaType
-- | Why does this only return a partial?  WTF is going on
part = do string "cd" 
          d <- digit
          return $! Part (read [d] :: Int)

pSeason :: Parser MediaType
pSeason = do
    char 'S'
    --Slow.  Change to takeWhile1
    x <- many1 digit
    return $! SeasonNo (read x :: Int)

pEpisode :: Parser MediaType
pEpisode = do
    char 'E'
    x <- many1 digit
    return $! EpisodeNo (read x :: Int)

title :: Parser MediaType
title = do
    skipMany attribute
    x <- manyTill anyChar $ choice [year, resolution, part, codec, source,  pEpisode, pSeason]
    return $! Title $! format $ T.pack x
    where
        format :: T.Text -> T.Text
        format title = T.strip $! T.unwords $! T.split (flip elem seperators) title

anyTill :: (Alternative f) => f a1 -> f a -> f a
-- |A copy of the ManyTill function that returns what succeeded at the end rather than what came before.
anyTill begin p = scan
    where scan = p <|> (begin *> scan)


choices :: Parser MediaType
choices = anyTill anyChar $! choice [codec, year, resolution, part, source, pEpisode, pSeason]

mainParse :: T.Text -> [MediaType]
mainParse toParse = 
    let 
        --If we don't match anything other than title we get a partial
        --This is very hacky but since <|> only accepts parsers of equal type.
        --end of file won't work here.  Since leftover data in the Done constructor
        --isn't used later that means we can feed whatever we like and it can be safely ignored.
        titulo :: MediaType
        titulo = fromJust.maybeResult $! feed (parse title toParse) "1984"
        otherVals :: [MediaType]
        otherVals = listFromJust.maybeResult $ feed (parse (many choices) toParse) ""
        listFromJust :: Maybe [MediaType] -> [MediaType]
        listFromJust x = case x of
            (Just a) -> a
            Nothing -> [] 
    in 
        sort (titulo:otherVals)

convertToSrc :: T.Text -> SourceDer
convertToSrc name 
    | nameU `elem` ["BDRIP", "BRRIP", "BLURAY"] = BD 
    | nameU == "HDDVD" = HDDVD
    | nameU == "HDTV" = HDTV
    | nameU `elem` ["DVDRIP", "NTSC", "PAL"] = DVD
    | nameU == "VHS" = VHS
    | nameU `elem` ["SCREENER", "R5"] =  PRE
    | nameU == "WEB-DL" = WEBDL
    where
        nameU = T.toUpper name

convertToCdc :: T.Text -> CodecDer
convertToCdc name 
    | nameU `elem` ["X264", "H264", "AVC"] = H264
    | nameU `elem` ["DIVX", "XVID"] = XVID
    | nameU == "VC-1" = VC1
    where
        nameU = T.toUpper name