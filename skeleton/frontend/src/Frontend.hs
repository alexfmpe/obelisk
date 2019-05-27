{-# LANGUAGE OverloadedStrings #-}

module Frontend where

import Control.Lens (FunctorWithIndex(..))
import Control.Monad.Fix
import Data.Functor.Alt
import Data.Functor.Bind

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route

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
