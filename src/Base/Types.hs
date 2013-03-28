module Base.Types where

import qualified Data.Text as T



data Movie = Movie {
                    mTitle :: T.Text, 
                    mYear :: Int,
                    mSource :: Maybe Source,
                    mEncoder :: Maybe T.Text,
                    mFiles :: [Part],
                    mNfo :: Maybe FilePath
                   }
             deriving (Eq, Show)

data Part = Part {
                    path :: FilePath,
                    partNo :: Int        
                 }
            deriving (Eq, Show)

data Source = DVD | BD | HDDVD | VHS | WEBDL | HDTV | PRE
            deriving (Eq, Show)

data Codec = H264 | VC1 | XVID
            deriving (Eq, Show)