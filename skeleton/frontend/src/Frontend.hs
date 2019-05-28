{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Applicative
import Control.Arrow
import Data.Align
import Data.Functor.Alt
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroupoid
import Control.Category
import Control.Monad
import Data.Profunctor (Profunctor(..))
import Data.These
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

instance (Adjustable t m, MonadHold t m) => Semigroupoid (Step t m) where
  Step b2c `o` Step a2b = mkStep $
    fmap (switch . current) . networkHold (pure never) . fmap b2c <=< a2b

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Category (Step t m) where
  id = arr id
  (.) = o

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Arrow (Step t m) where
  arr a2b = mkStep $ ffor getPostBuild . fmap . const . a2b

  Step a2b *** Step c2d = mkStep $ \(a,c) -> do
    let holdJust = holdDyn Nothing . fmap Just
    ffor2 (holdJust =<< a2b a) (holdJust =<< c2d c) $ \b d ->
      fmapMaybe id $ updated $ (liftA2 . liftA2) (,) b d

instance (Reflex t, MonadHold t m) => Profunctor (Step t m) where
  dimap f g (Step a2b) = fmap g $ mkStep (a2b . f)

instance (Functor m, Reflex t, Arrow (Step t m)) => Apply (Step t m a) where
  liftF2 f a2b a2c = fmap (uncurry f) $ a2b &&& a2c

instance (Functor m, Reflex t, Arrow (Step t m)) => Applicative (Step t m a) where
  pure = arr . const
  liftA2 = liftF2

instance (Apply m, Reflex t, MonadHold t m) => Alt (Step t m a) where
  Step f <!> Step g = mkStep $ (liftF2 . liftF2) (<!>) f g

instance (Apply m, Applicative m, Reflex t, MonadHold t m, Arrow (Step t m)) => Alternative (Step t m a) where
  empty = nil
  (<|>) = (<!>)


instance (Adjustable t m, MonadHold t m, PostBuild t m) => Bind (Step t m a) where
  join (Step a2sb) = mkStep $ \a -> do
    evSb <- a2sb a
    fmap (switch . current) $ networkHold (pure never) $ ffor evSb $ \(Step a2b) -> a2b a


#if MIN_VERSION_these(0, 8, 0)
instance (Reflex t, Applicative m) => Align (Step t m a) where
  nil = Step $ const $ pure never
instance (Reflex t, Apply m) => Semialign (Step t m a) where
  align (Step a2b) (Step a2c) = Step $ (liftA2 . liftA2) align a2b a2c
#else
instance (Reflex t, Applicative m) => Align (Step t m a) where
  nil = Step $ const $ pure never
  align (Step a2b) (Step a2c) = Step $ (liftA2 . liftA2) align a2b a2c
#endif

instance (Semigroup b, Reflex t, Applicative m) => Semigroup (Step t m a b) where
  f <> g = alignWith (mergeThese (<>)) f g
--  sconcat = fmap sconcat . mergeList . toList
  stimes n = fmap $ stimes n

instance (Semigroup b, Reflex t, Applicative m) => Monoid (Step t m a b) where
  mempty = nil
  mappend = (<>)
--  mconcat = fmap sconcat . mergeList
