{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Applicative
import Control.Arrow
import Control.Lens (FunctorWithIndex(..))
import Control.Monad.Fix
import Data.Align
import Data.Functor.Alt
import Data.Functor.Bind
import Data.List.NonEmpty
import Data.These

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      clk <- button "simultaneous"
      br
      let
        w2 = wn clk 2 0
        w3 = wn clk 3 0

      workflow w2 >>= display
      br
      workflow w3 >>= display
      br

      renderW "<>" $ fmap show w2 <> pure " " <> fmap show w3
      renderW "<.>" $ (,) <$> w2 <*> w3
      renderW "<!>" $ w2 <!> w3
      renderW "join" $ join $ ww clk 5 0
  }

tshow :: Show a => a -> T.Text
tshow = T.pack . show

wn :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> Workflow t m Int
wn ev n i = Workflow $ do
  inc <- button $ T.pack $ show i <> "/" <> show n
  pure (i, wn ev n ((i + 1) `mod` n) <$ leftmost [ev, inc])

ww :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> Workflow t m (Workflow t m Int)
ww ev n i = Workflow $ do
  next <- button "next"
  pure (wn ev (i + 1) 0, ww ev n ((i + 1) `mod` n) <$ leftmost [ev, next])

br :: DomBuilder t m => m ()
br = el "br" blank

renderW :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Show a) => T.Text -> Workflow t m a -> m ()
renderW lbl w = do
  text lbl
  ipayload <- workflow $ imap (,) w
  dynText $ ffor ipayload $ \(k, p) -> "[" <> tshow k <> "] " <> tshow p
  br

instance Monad m => Apply (RoutedT t r m) where
  (<.>) = (<*>)



ffor2' :: Apply f => f a -> f b -> (a -> b -> c) -> f c
ffor2' a b f = liftF2 f a b

deriving instance (Functor m, Reflex t) => Functor (Workflow t m)

zipNEListWithWorkflow :: (Monad m, Reflex t) => NonEmpty k -> Workflow t m a -> Workflow t m (k, a)
zipNEListWithWorkflow (k :| ks) w = Workflow $ do
  (a0, wEv) <- unWorkflow w
  pure ((k, a0), case nonEmpty ks of
           Nothing -> never
           Just nel -> zipNEListWithWorkflow nel <$> wEv)

instance (Monad m, Reflex t) => FunctorWithIndex Int (Workflow t m) where
  imap f w = uncurry f <$> zipNEListWithWorkflow (0 :| [1..]) w

instance (Apply m, Reflex t) => Apply (Workflow t m) where
  liftF2 f = parallelWorkflows f f f

instance (Apply m, Applicative m, Reflex t) => Applicative (Workflow t m) where
  pure a = Workflow $ pure (a, never)
  (<*>) = (<.>)

instance (Apply m, Reflex t, Semigroup a) => Semigroup (Workflow t m a) where
  (<>) = liftF2 (<>)

instance (Apply m, Applicative m, Reflex t, Monoid a) => Monoid (Workflow t m a) where
  mempty = pure mempty

instance (Apply m, Reflex t) => Alt (Workflow t m) where
  (<!>) = parallelWorkflows const (flip const) const

zipWorkflows :: (Apply m, Reflex t) => Workflow t m a -> Workflow t m b -> Workflow t m (a,b)
zipWorkflows = parallelWorkflows (,) (,) (,)

parallelWorkflows :: (Apply m, Reflex t)
                  => (a -> b -> c) -> (a -> b -> c) -> (a -> b -> c)
                  -> Workflow t m a -> Workflow t m b -> Workflow t m c
parallelWorkflows fL fR fLR = go fLR
  where
    go f0 wl wr = Workflow $ ffor2' (unWorkflow wl) (unWorkflow wr) $ \(l0, wlEv) (r0, wrEv) ->
      (f0 l0 r0, ffor (align wlEv wrEv) $ \case
          This wl' -> go fL wl' wr
          That wr' -> go fR wl wr'
          These wl' wr' -> go fLR wl' wr'
      )

instance (Apply m, Monad m, Reflex t) => Bind (Workflow t m) where
  join wwa = Workflow $ do
    let replaceInitial a wa = Workflow $ first (const a) <$> unWorkflow wa
    (wa0, wwaEv) <- unWorkflow wwa
    (a0, waEv) <- unWorkflow wa0
    pure (a0, join <$> leftmost [wwaEv, flip replaceInitial wwa <$> waEv])

instance (Apply m, Monad m, Reflex t) => Monad (Workflow t m) where
  (>>=) = (>>-)
