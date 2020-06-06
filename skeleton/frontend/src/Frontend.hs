{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence
import Data.Text (Text)
import Data.Traversable
import Data.Tree
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Unsafe.Coerce

import Prelude hiding (div, head)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core hiding (Attributes)

import Common.Api
import Common.Route

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      let beforeAfter ev = runWithReplace (el "p" $ text "before") $ ffor ev $ \_ -> el "p" $ text "after"

      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff

      ticks <- tickLossyFromPostBuildTime 2
      beforeAfter ticks

      prerender_ blank $ do
        html <- root
        tick <- tickLossyFromPostBuildTime 0.3

        beforeAfter ticks

        widgetHold_ blank $ ffor tick $ \_ -> do
          tree <- drawTree <$> currentTree html
          el "pre" . text . T.pack $ tree

      return ()
  }

{- Example -}
example
  :: (MonadFix m, MonadIO m, MonadIO (Performable m), MonadHold t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, Adjustable t m)
  => DOMNode m -> m (Dynamic t Int)
example parent = do
  div <- el_ parent "div" mempty
  _ <- el_ div "input" mempty

  but1 <- el_ parent "button" $ "background" =: "blue"
  but2 <- el_ div "button" $ "background" =: "red"
  let clickEv = leftmost [click but1, click but2]
  clicks <- foldDyn (+) 0 $ 1 <$ clickEv

  let
    w0 anchor cast = do
      -- el_ parent "MUST NOT TYPECHECK" mempty --     • Couldn't match type ‘localHierarchy’ with ‘m’
      p <- anchor "div" ("placeholder" =: "true")
      fmap cast $ el_ p "div" ("placeholder" =: "true")

    wEv anchor b = do
      when b $ gallery anchor
      -- el_ parent "MUST NOT TYPECHECK" mempty --     • Couldn't match type ‘m’ with ‘Performable m’
      pure b

  tick <- tickLossyFromPostBuildTime 1
  tg <- toggle True tick

  runWithReplace (el_ parent "p" mempty) $ ffor tick $ \_ -> blank

  (_n0, _nEv) <- elAdjustable_ parent w0 $ \anchor -> ffor (updated tg) $ wEv anchor

  pure clicks

gallery :: MonadIO m => Anchor m h -> m ()
gallery anchor = do
  for_ ["A", "B", "C"] $ \src -> do
    anchor "img" $ "src" =: (src <> ".png")

root
  :: (MonadFix m, MonadIO m, MonadIO (Performable m), MonadHold t m, PerformEvent t m, PostBuild t m, TriggerEvent t m, Adjustable t m)
  => m (DOMNode m)
root = mkRoot $ \(_h,b) -> example b

{- Stubs -}
type Tag = Text
type Children h = Seq (IORef (Seq (DOMNode h)))
type Attributes = Map Text Text
type Anchor m h = Tag -> Attributes -> m (DOMNode h)

data DOMNode h = DOMNode Tag (IORef Attributes) (IORef (Children h))
newtype Coercer a = Coercer a

click :: Reflex t => DOMNode h -> Event t ()
click _ = never

el_ :: MonadIO m => DOMNode m -> Anchor m m
el_ (DOMNode _ _ childrenRef) tag' attrs = do
  n <- mkNode tag' attrs
  liftIO $ do
    ref <- newIORef $ singleton n
    modifyIORef childrenRef (|> ref)
  pure n

elAdjustable_
  :: (MonadIO m, MonadIO n, MonadIO (Performable m), PerformEvent t m)
  => DOMNode m
  -> (
     forall localHierarchy                    -- name for error messages
     .  MonadIO localHierarchy
     => Anchor localHierarchy localHierarchy  -- root of local hierarchy
     -> (DOMNode localHierarchy -> DOMNode m) -- cast for returning nodes
     -> localHierarchy a
     )
  -> (Anchor n m -> Event t (Performable m b))
  -> m (a, Event t b)
elAdjustable_ (DOMNode _ _ childrenRef) w0 wEv = do
  slice <- liftIO $ newIORef mempty
  liftIO $ modifyIORef childrenRef (|> slice)

  let
    castNode = unsafeCoerce

    anchor t a = do
      n <- fmap castNode $ mkNode t a
      liftIO $ modifyIORef slice (|> n)
      pure n

  n0 <- w0 anchor castNode

  ev' <- performEvent $ ffor (wEv anchor) $ \x -> do
    liftIO $ writeIORef slice mempty
    x

  pure (n0, ev')

mkRoot :: MonadIO m => ((DOMNode m, DOMNode m) -> m a) -> m (DOMNode m)
mkRoot f = do
  html <- mkNode "html" mempty
  h <- el_ html "head" mempty
  b <- el_ html "body" mempty
  _ <- f (h,b)
  pure html

mkNode :: MonadIO m => Tag -> Map Text Text -> m (DOMNode m)
mkNode tag' attrs = liftIO $ DOMNode tag' <$> newIORef attrs <*> newIORef mempty


{- Debug -}
currentTree :: MonadIO m => DOMNode h -> m (Tree String)
currentTree (DOMNode tag' attrsRef childrenRef) = liftIO $ do
  attrs <- readIORef attrsRef
  children <- traverse readIORef =<< readIORef childrenRef
  let keyvalue (k,v) = k <> if T.null v
                            then ""
                            else "=\"" <> v <> "\""
      attrs' = T.unpack $ fold $
        [ "<"
        , tag'
        , case Map.toList attrs of
            [] -> ""
            as -> " " <> T.unwords (ffor as keyvalue) <> ""
        , ">"
        ]

  Node attrs' . toList <$> traverse currentTree (join children)
