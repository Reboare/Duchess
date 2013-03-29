{-#LANGUAGE OverloadedStrings#-}

module Text.AttoParser where

import Base.Types
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import System.FilePath


attribute :: Parser T.Text
attribute = do
    char '['
    res <- takeWhile1 (']' /=)
    char ']'
    return res

year :: Parser T.Text
year =  do
    year <- count 4 digit
    return $ (T.pack year)

videoSource :: Parser T.Text
videoSource = do
              src <- choice $! map asciiCI ["BDRip", "BRRip", "BluRay",
                                            "HDDVD", "HDTV", "DVDRip", 
                                             "VHS", "Screener", "R5", "WEB-DL"]
              return $! src
                

sourceCodec :: Parser T.Text
sourceCodec = do
            cdc <- choice $! map asciiCI ["xvid", "x264", "h264", "divx", 
                                         "AVC", "VC-1"]
            return $! cdc

resolution :: Parser T.Text
resolution = do
            res <- T.pack <$> (many digit)
            inc <- (char 'p' <|> char 'i')
            return (T.append res (T.singleton inc))

parseCodec :: T.Text -> Maybe Codec 
parseCodec res = case parse sourceCodec res of 
                    Fail t _ _ -> Nothing
                    Partial t -> Nothing
                    Done t res -> Just $! convertToCdc res

parseSource :: T.Text -> Maybe Source
parseSource res = case parse videoSource res of
                    Fail t _ _ -> Nothing
                    Partial t -> Nothing
                    Done t res -> Just $! convertToSrc res

getYear xs = case parse (year) xs of
                    Fail t _ _ -> Nothing
                    Partial t -> Nothing
                    Done t res -> Just (read (T.unpack res) :: Int)

title :: Parser T.Text
title = T.pack <$> manyTill (anyChar) (choice [year, sourceCodec, videoSource])

recParse :: T.Text -> Movie 
recParse filepath = case parse title filename of
                        Fail t _ _ -> Movie filename Nothing Nothing Nothing [Part filepath 1]
                        Partial t -> Movie filename Nothing Nothing Nothing [Part filepath 1]
                        Done _ res -> Movie (format res) (parseOver getYear filename) (parseOver parseSource filename) Nothing [Part filepath 1] 
                     where
                        filename :: T.Text
                        filename = T.pack $! fst $! splitExtension  $! takeFileName  $! T.unpack filepath

parseOver :: Eq a => (T.Text -> Maybe a) -> T.Text -> Maybe a                       
parseOver parser filename = head $! filter (Nothing /=) $ map parser (T.split (flip elem seperators) filename)

format :: T.Text -> T.Text
format title = T.strip $! T.unwords $! T.split (flip elem seperators) title


seperators = ". _-"

scene :: String
scene = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._()"

sceneStandard :: String -> Bool
sceneStandard xs = all ((flip elem) scene) xs

convertToSrc name 
    | name `elem` ["BDRip", "BRRip", "BluRay"] = BD 
    | name == "HDDVD" = HDDVD
    | name == "HDTV" = HDTV
    | name == "DVDRip" = DVD
    | name == "VHS" = VHS
    | name `elem` ["Screener", "R5"] =  PRE
    | name == "WEB-DL" = WEBDL

convertToCdc name 
    | name `elem` ["x264", "h264", "AVC"] = H264
    | name `elem` ["divx", "xvid"] = XVID
    | name == "VC-1" = VC1