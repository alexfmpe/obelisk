{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import qualified Language.C.Inline as C

C.include "<math.h>"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      putStrLn . show =<< [C.exp| double{ cos(1) } |]
      putStrLn . show =<< [C.exp| double{ sin(1) } |]
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
