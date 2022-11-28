{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
module Backend where

import Control.Concurrent
import Control.Monad.State
import Data.Text (Text)
import System.IO

import Common.Route
import Common.Serialization
import Obelisk.Backend
import Obelisk.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> do
      threadDelay 1e4

      putStrLn mempty
      putStrLn $ explain ex1

      case checkEncoder (toEncoderImplBytes ex1) of
        Left err -> print err
        Right enc -> do
          let
            e = encode @(StateT Word (Either Text)) enc $
              ((Ace, Spades), ((Queen, Hearts), (Two, Clubs)))
              --((Ace, Spades) /\ (Queen, Hearts) /\ (Two, Clubs))
            d = flip evalStateT 0 $ tryDecode enc $ e
          print e
          print d
  , _backend_routeEncoder = fullRouteEncoder
  }


{-
ex1 :: Format Card Word8
ex1 = card (Ace, Spades)
   /\ card (Queen, Hearts)
   /\  (  card (Two, Diamonds) /\ card (Three, Diamonds)
       \/ card (Two, Clubs)    /\ card (Three, Clubs)
       )
-}
