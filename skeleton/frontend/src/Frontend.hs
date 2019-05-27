{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Profunctor (Profunctor(..))
import qualified Data.Text as T
import Prelude hiding (id, (.))

import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Network

import Common.Api
import Common.Route
import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
  }

newtype Step t m a b = Step (a -> m (Event t b)) deriving Functor

mkStep :: MonadHold t m => (a -> m (Event t b)) -> Step t m a b
mkStep a2b = Step $ headE <=< a2b

dup :: a -> (a,a)
dup a = (a,a)

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Category (Step t m) where
  id = arr id

  Step b2c . Step a2b = mkStep $
    fmap (switch . current) . networkHold (pure never) . fmap b2c <=< a2b

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Arrow (Step t m) where
  arr a2b = mkStep $ ffor getPostBuild . fmap . const . a2b

  Step a2b *** Step c2d = mkStep $ \(a,c) -> do
    let holdJust = holdDyn Nothing . fmap Just
    ffor2 (holdJust =<< a2b a) (holdJust =<< c2d c) $ \b d ->
      fmapMaybe id $ updated $ (liftA2 . liftA2) (,) b d

instance (Reflex t, MonadHold t m) => Profunctor (Step t m) where
  dimap f g (Step a2b) = fmap g $ mkStep (a2b . f)

instance (Functor m, Reflex t, Arrow (Step t m)) => Applicative (Step t m a) where
  pure = arr . const
  liftA2 f a2b a2c = fmap (uncurry f) $ a2b &&& a2c
