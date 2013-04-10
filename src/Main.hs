{-#LANGUAGE OverloadedStrings#-}
module Main where

import Identifiers.Infer
import Base.Types
import Base.HTTP
import Data.Time.Clock
import qualified Data.Text as T
import System.FilePath
import Data.Attoparsec.Text

main =do
    a <- getCurrentTime  
    b <- getCurrentTime 
    let time = diffUTCTime b a
    print $ infer "Eternal.Sunshine.of.the.Spotless.Mind.2004.720p.BluRay.DTS.x264-CtrlHD"

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