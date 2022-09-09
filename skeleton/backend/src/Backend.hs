{-# LANGUAGE TemplateHaskell #-}
module Backend where

import Backend.TH
import Common.Route
import Data.ByteString
import Data.FileEmbed
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

contents :: ByteString
contents = $(embedFile path)
