{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Guesses various properties about a film from it's filename
module Identifiers.Infer
(
    infer
)
where

import           Base.Types
import           Control.Applicative
import           Data.List
import           Data.Maybe
import qualified Data.Text                as T
import           Identifiers.Infer.Parser 
import           System.FilePath
import           Identifiers.Infer.LegacyMove

infer :: FilePath -> MediaFile
-- |Constructor for a Media Type from a filename
infer filepath =
    convertBetween (Movie attrs) filepath
    where
        filename :: T.Text
        filename = T.pack $! filepath
        -- Removed takeBaseNamein the case of folders
        -- I'll Let the Caller take car of that part
        -- Better to keep this independent of the filesystem
        attrs :: [MediaType]
        attrs = mainParse filename
        
--Note: Support for multi-part mkv's might be supported
--MIGHT BE!

isEpisode :: [MediaType] -> Bool
isEpisode xs =
    any eqEpType $! xs
    where
        eqEpType (SeasonNo a) = True
        eqEpType (EpisodeNo a) = True
        eqEpType _ = False

main = 
    print $!  infer "Grandma's Boy"

convertBetween :: Media -> String -> MediaFile
convertBetween attrs filepath =
    MediaFile
        filepath 
        nMediaInfo
        (res' attrs)
        (source' attrs)
        (codec' attrs)
        (part' attrs)
    where
        nMediaInfo =
            NMedia
                (fromJust.title' $ attrs)
                (year' attrs)
                (runtime' attrs)
                (imdbID' attrs)
                (imdbRating' attrs)
                (synopsis' attrs)
                (actors' attrs)
                (episode' attrs) 
                (season' attrs) 
                (poster' attrs)