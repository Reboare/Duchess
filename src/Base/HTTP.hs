{-#LANGUAGE OverloadedStrings#-}
module Base.HTTP
(
    Url,
    get,
    getHTML,
    parseDomain,
    validateLink
)
where

import qualified Codec.Compression.GZip     as GZip (decompress)
import           Control.Monad              (liftM)
import qualified Data.ByteString.Lazy       as L (ByteString (..), take)
import qualified Data.ByteString.Lazy.Char8 as L1 (pack)
import           Data.Char                  (toLower)
import           Data.Maybe                 (fromJust)
import           Network.HTTP               
import           Network.URI                (URI (..), parseURI, uriRegName)

type Url = String

data HMangaError = HeaderError deriving (Show)


getX uri = simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

-- |Default Implementation of a GET Request.
-- |Note: This Currently throws an error when an invalid url is passed.  This will not be changed.
get url = getX (fromJust.parseURI $ url)


getHTML :: String -> IO L.ByteString
getHTML url = do
    let uri = fromJust.parseURI $ url

    let userAgent = mkHeader HdrUserAgent "haskell-HTTP/4000.2.8"
    let contentLength = mkHeader HdrContentLength "0"
    let acceptEncoding = mkHeader HdrAcceptEncoding "gzip"

    let reqBody = "" :: L.ByteString

    response <- simpleHTTP (Request uri GET [userAgent, contentLength, acceptEncoding] reqBody)
    case response of
        (Right a) -> case findHeader HdrContentEncoding a of
                                Just "gzip" -> liftM GZip.decompress $! getResponseBody response
                                otherwise -> getResponseBody response
        otherwise -> error "Whatever!" {-FIX THIS SOON SON-}


validateLink :: String -> Bool
validateLink url =
    dom `elem` ["mangafox.me" , "www.batoto.net", "kissmanga.com", "www.hbrowse.com"]
    where
        dom = parseDomain url


parseDomain :: Url -> Url
parseDomain url =
    let
        work = parseURI url >>= uriAuthority
    in
        case work  of
            (Just a) -> map toLower (uriRegName a)
            Nothing -> ""