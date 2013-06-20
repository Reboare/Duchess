{-# LANGUAGE OverloadedStrings #-}
module Renamer

where

import           Base.Types
import           Control.Lens
import           Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text       as AT
import qualified Data.Text                  as T
import qualified System.Directory           as D
import           System.FilePath
import           Renamer.TitleFormat
import           Identifiers
import           Control.Monad

type FormatString = String

{-
%t title
%y year
%len runtime
%irat imdbrating
%id imdbid
-}
baseMedia :: NMedia
baseMedia = NMedia "Grandma's Boy" (Just 2006) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
baseFile :: MediaFile
baseFile = MediaFile "" baseMedia Nothing Nothing Nothing Nothing


rename :: FormatString -> FilePath -> IO ()
rename fmt path = undefined

httpGenRename :: FormatString -> String -> IO T.Text
httpGenRename fmt name = liftM (genRenamer' (T.pack fmt)) $! inferIMDB name

main = print =<< httpGenRename "$title [($year)]" "stoker.2013.720p.web-dl.h264-publichd"

