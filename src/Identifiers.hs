{-# LANGUAGE OverloadedStrings #-}
module Identifiers 
(
infer,
inferIMDB,
updateInfo
)
where

import           Base.Types
import           Control.Lens
import           Control.Monad
import           Data.Functor
import           Data.Maybe
import           Identifiers.IMDB
import           Identifiers.Infer

main = print =<< inferIMDB "Eternal.Sunshine.of.the.Spotless.Mind.2004.720p.BluRay.DTS.x264-CtrlHD"

inferIMDB :: FilePath -> IO MediaFile
inferIMDB = updateInfo.infer