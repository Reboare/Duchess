{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Base.HTTP
import           Base.Types
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           Data.Time.Clock
import           Identifiers.Infer
import           Renamer
import           System.FilePath

myfmt = "%t (%y)"

main =do
    a <- getCurrentTime
    let c = map (genRename myfmt) abso
    print c
    b <- getCurrentTime
    let time = diffUTCTime b a
    print time

abso = ["Eternal.Sunshine.of.the.Spotless.Mind.2004.720p.BluRay.DTS.x264-CtrlHD",
        "From.Dusk.Till.Dawn.1996.MKV.x264",
        "Grindhouse.2007.720p.BluRay.x264.SiNNERS",
        "Heat.1995.720p.BluRay.x264-CtrlHD",
        "Incendies.2010.720p.BluRay.x264-HaB",
        "Iron.Man.2008.720p.BluRay.x264-EbP"]
