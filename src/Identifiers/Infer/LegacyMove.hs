module Identifiers.Infer.LegacyMove

where
import Identifiers.Infer.Parser
import qualified Data.Text as T
import Base.Types

-- Lots of boilerplate but the disadvantages of Template Haskell made me move over to this
title' :: [MediaType] -> Maybe T.Text
title' (x:xs) = case x of
    (Title a) -> Just a
    _ -> title' xs
title' [] = Nothing

year' :: [MediaType] -> Maybe Int 
year' (x:xs) = case x of
    (Year a) -> Just a
    _ -> year' xs
year' [] = Nothing

res' :: [MediaType] -> Maybe ResolutionDer
res' (x:xs) = case x of
    (Resolution a) -> Just a
    _ -> res' xs
res' [] = Nothing

source' :: [MediaType] -> Maybe SourceDer
source' (x:xs) = case x of
    (Source a) -> Just a
    _ -> source' xs
source' [] = Nothing

codec' :: [MediaType] -> Maybe CodecDer
codec' (x:xs) = case x of
    (Codec a) -> Just a
    _ -> codec' xs
codec' [] = Nothing

part' :: [MediaType] -> Maybe Int
part' (x:xs) = case x of
    (Part a) -> Just a
    _ -> part' xs
part' [] = Nothing

runtime' :: [MediaType] -> Maybe Int
runtime' (x:xs) =  case x of
    (Runtime a) -> Just a
    _ -> part' xs
runtime' [] = Nothing

imdbID' :: [MediaType] -> Maybe Int
imdbID' (x:xs) = case x of
    (IMDBid a) -> Just a
    _ -> imdbID' xs
imdbID' [] = Nothing

imdbRating' :: [MediaType] -> Maybe Float
imdbRating' (x:xs) = case x of
    (IMDBRating a) -> Just a
    _ -> imdbRating' xs
imdbRating' [] = Nothing

synopsis' :: [MediaType] -> Maybe T.Text
synopsis' (x:xs) = case x of
    (Synopsis a) -> Just a
    _ -> synopsis' xs
synopsis' [] = Nothing

actors' :: [MediaType] -> Maybe [Actor]
actors' (x:xs) = case x of
    (Actors a) -> Just a
    _ -> actors' xs
actors' [] = Nothing

episode' :: [MediaType] -> Maybe Int
episode' (x:xs) = case x of
    (EpisodeNo a) -> Just a
    _ -> episode' xs
episode' [] = Nothing

season' :: [MediaType] -> Maybe Int
season' (x:xs) = case x of
    (SeasonNo a) -> Just a
    _ -> season' xs
season' [] = Nothing

poster' :: [MediaType] -> Maybe (Either String String)
poster' (x:xs) = case x of
    (Poster a) -> Just a
    _ -> poster' xs
poster' [] = Nothing

