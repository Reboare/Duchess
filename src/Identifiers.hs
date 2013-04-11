{-#LANGUAGE OverloadedStrings#-}
module Identifiers 
(
    
    findMovies,
    SearchMode(..)
)
where

import Identifiers.IMDB
import Identifiers.Infer
import System.FilePath
import System.Directory
import Data.Maybe (fromJust)
import System.FilePath.Glob
import Data.Type.Equality
exts = ["*.mkv", "*.avi", "*.mp4"]

data SearchMode = Recursive --Every mkv or avi in every sub folder 
                 |HeadFile --Only file in top level 
                 |HeadItem --A folder matches a movie as well

findMovies :: FilePath -> SearchMode -> IO [FilePath]
findMovies folder mode = case mode of
                            Recursive -> undefined
                            HeadFile -> headFileSearch folder
                            HeadItem -> undefined

dirContents :: FilePath -> IO [FilePath]
dirContents folder = do
    dirfiles <- getDirectoryContents folder
    return.init.init $! map (\x -> joinPath [folder, x]) dirfiles

headFileSearch :: FilePath -> IO [FilePath]
headFileSearch folder = do
    files <- namesMatching $! joinPath [folder, "*.mkv"]
    files1 <- namesMatching $! joinPath [folder, "*.avi"]
    files2 <- namesMatching $! joinPath [folder, "*.mp4"]
    return $! files++files1++files2

main = print $! infer "[sage]_Hellsing_Ultimate_OVA_-_10_[720p-AAC][4B4C84C5]"

