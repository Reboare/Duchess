{-# LANGUAGE OverloadedStrings #-}
module Identifiers 
where

import           Base.Types
import           Control.Lens
import           Control.Monad
import           Data.Functor
import           Data.Maybe
import           Identifiers.Infer

main = print.infer $! "Hellbound.Hellraiser.II.1988.720p.BluRay.x264-PSYCHD"