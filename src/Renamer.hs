{-# LANGUAGE OverloadedStrings #-}
module Renamer
(
    rename,
    genRename
)
where

import           Base.Types
import           Control.Lens
import qualified Data.Text         as T
import           Identifiers.Infer
import qualified System.Directory as D
import System.FilePath


type FormatString = String

{-
%t title
%y year
%len runtime
%irat imdbrating
%id imdbid
-}
rename :: FormatString -> FilePath -> IO ()
rename fmt path = D.renameFile path $! replaceBaseName path newName
    where
        newName = genRename fmt (takeBaseName path)


--This is very basic at the moment.  Hope to include a full formatting language soon
genRename :: FormatString -> String -> String
genRename fmt str = genRename' fmt (view info$! infer str)

genRename' :: FormatString -> NMedia -> String
genRename' (x:str) nMedia = case x of
    '%' -> case take 1 str of
        "y" -> rYear ++ (genRename' (drop 1 str) nMedia)
        "t" -> rTitle ++ (genRename' (drop 1 str) nMedia)
        "l" -> case take 3 str of
            "len" -> rLen ++ (genRename' (drop 3 str) nMedia)
            _ -> x: genRename' str nMedia
        "i" -> case take 2 str of
            "id" -> rId ++ (genRename' (drop 2 str) nMedia)
            "ir" -> case take 4 str of
                "irat" -> rIrat ++ (genRename' (drop 4 str) nMedia)
                _ -> x : genRename' str nMedia
    _ -> x : genRename' str nMedia
    where 
        rTitle = T.unpack $!view title nMedia
        rYear = unpackJust $! view year nMedia
        rLen = unpackJust $! view runtime nMedia
        rIrat = unpackJust $! view imdbrating nMedia
        rId = unpackJust $! view imdbid nMedia
        unpackJust (Just a) = show a
        unpackJust Nothing = ""
genRename' [] _ = []


