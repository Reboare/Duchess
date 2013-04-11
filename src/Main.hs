{-#LANGUAGE OverloadedStrings#-}
module Main where

import Identifiers.Infer
import Identifiers.IMDB

import Base.Types
import Base.HTTP
import Data.Time.Clock
import qualified Data.Text as T
import System.FilePath
import Data.Attoparsec.Text
import Control.Monad
import Control.Applicative

main2 =do
    a <- getCurrentTime  
    let c = map infer abso
    print c
    b <- getCurrentTime 
    let time = diffUTCTime b a
    print time

main = do
    x <- (getTitle "True Grit" (Just 2010) Nothing)
    print x


abso = ["Eternal.Sunshine.of.the.Spotless.Mind.2004.720p.BluRay.DTS.x264-CtrlHD",
        "From.Dusk.Till.Dawn.1996.MKV.x264",
        "Grindhouse.2007.720p.BluRay.x264.SiNNERS",
        "Heat.1995.720p.BluRay.x264-CtrlHD",
        "Incendies.2010.720p.BluRay.x264-HaB",
        "Iron.Man.2008.720p.BluRay.x264-EbP",
        "Eternal.Sunshine.of.the.Spotless.Mind.2004.720p.BluRay.DTS.x264-CtrlHD",
        "From.Dusk.Till.Dawn.1996.MKV.x264",
        "Grindhouse.2007.720p.BluRay.x264.SiNNERS",
        "Heat.1995.720p.BluRay.x264-CtrlHD",
        "Incendies.2010.720p.BluRay.x264-HaB",
        "Iron.Man.2008.720p.BluRay.x264-EbP"]