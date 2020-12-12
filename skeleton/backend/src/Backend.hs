module Backend where

import Common.Encoders
import Common.Route
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      test
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
