module Base.Types where

import qualified Data.Text as T



data Movie = Movie {
                    mTitle :: T.Text, 
                    mYear :: Maybe Int,
                    mSource :: Maybe Source,
                    mEncoder :: Maybe String,
                    mFiles :: [Part]
                   }
             deriving (Eq)

instance Show Movie where
    show (Movie title year src enc fil) = (showTitle title) ++ showYear year ++ showSrc src ++ 
                                          showEnc enc ++ (foldl1 (++) $! map ("\n\t"++) (map show fil))
                        where 
                            showTitle a = take (T.length a ) $ drop 1 (show a)
                            showYear a = case a of
                                            (Just b) -> '(' : (show b) ++ ")"
                                            Nothing -> ""
                            showSrc a = case a of
                                            (Just b) -> ' ':(show b)
                                            Nothing -> ""
                            showEnc a = case a of
                                            (Just b) -> ' ':(show b)
                                            Nothing -> ""

data Part = Part {
                    path :: T.Text,
                    partNo :: Int        
                 }
            deriving (Eq)

instance Show Part where
    show (Part pth no) = show no ++ ". " ++ (showTitle pth)
                    where
                        showTitle a = take (T.length a + 2) $ drop 1 (show a)

data Source = DVD | BD | HDDVD | VHS | WEBDL | HDTV | PRE
            deriving (Eq, Show)

data Codec = H264 | VC1 | XVID
            deriving (Eq, Show)

data Group = ESiR | TBB | HANDJOB 
            deriving (Eq, Show)