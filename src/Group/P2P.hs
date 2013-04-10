{-#LANGUAGE OverloadedStrings#-}
module Group.P2P where

import qualified Data.Text as T

p2pGroups :: [T.Text]
-- |All Groups in this list generally have an excellent record of encodes
-- |If they are the encoder it is reasonable to assume they did a good job
p2pGroups = ["TBB", "HaB", "CRiSC", "CtrlHD", "DON", "EbP", "ESiR", "fty",
             "THORA", "Skazhutin", "Chotab", "XTSF", "D-Z0N3", "FoRM",
             "decibeL", "de[42]"]

isQuality :: T.Text -> Bool
isQuality xs = T.strip (last $ T.split ('-' ==) xs ) `elem` p2pGroups 

main = putStrLn "Hello World"