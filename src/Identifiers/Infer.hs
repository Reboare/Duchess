{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE FlexibleInstances#-}
-- |Guesses various properties about a film from it's filename
module Identifiers.Infer 
(
    infer
)
where

import System.FilePath
import Data.Maybe
import Data.List
import Control.Applicative
import qualified Data.Text as T

import Identifiers.Infer.Parser
import Base.Types


infer :: FilePath -> Media
-- |Constructor for a Media Type from a filename
infer filepath = 
    case isEpisode(attrs) of
        True -> Episode attrs
        False -> Movie attrs
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
    not.null.(filter eqEpType) $! xs
    where
        eqEpType (SeasonNo a) = True
        eqEpType (EpisodeNo a) = True
        eqEpType _ = False