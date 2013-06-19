{-# LANGUAGE OverloadedStrings #-}
module Renamer where

import Base.Types
import Control.Lens
import qualified Data.Text as T


rename :: String -> NMedia -> String
rename (x:str) nMedia = case x of
    '%' -> case take 1 str of
        "y" -> rYear ++ (rename (drop 1 str) nMedia)
        "t" -> rTitle ++ (rename (drop 1 str) nMedia)
        "l" -> case take 3 str of
            "len" -> rLen ++ (rename (drop 3 str) nMedia)
            _ -> rename str nMedia
        "i" -> case take 2 str of
            "id" -> rId ++ (rename (drop 2 str) nMedia)
            "ir" -> case take 4 str of
                "irat" -> rIrat ++ (rename (drop 4 str) nMedia)
                _ -> rename str nMedia
    where 
        rTitle = T.unpack $!view title nMedia
        rYear = show $! view year nMedia
        rLen = show $! view runtime nMedia
        rIrat = show $! view imdbrating nMedia
        rId = show $! view imdbid nMedia
rename [] _ = []


