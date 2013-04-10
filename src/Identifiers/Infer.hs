{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE FlexibleInstances#-}
-- |Guesses various properties about a film from it's filename
module Identifiers.Infer 
(
    infer
)
where

--TODO
--Change the api to allow returning of 

import System.FilePath
import Data.Maybe
import Data.List
import Control.Applicative
import qualified Data.Text as T

import Identifiers.Infer.Parser
import Base.Types


infer :: FilePath -> Media
-- |Constructor for a movie type from a filename
infer filepath = 
    case isEpisode(attrs) of
        True -> Episode attrs
        False -> Movie attrs
    where
        filename :: T.Text
        filename = T.pack $! takeBaseName filepath
        attrs :: [MediaType]
        attrs = mainParse filename

isEpisode :: [MediaType] -> Bool
isEpisode xs = 
    not.null.(filter eqEpType) $! xs
    where
        eqEpType (SeasonNo a) = True
        eqEpType (EpisodeNo a) = True
        eqEpType _ = False