{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s

      prerender_ blank $ do
        html <- root
        tree <- drawTree <$> currentTree html
        el "pre" . text . T.pack $ tree

      return ()
  }

{- Example -}
example :: (MonadFix m, MonadIO m, MonadHold t m, Reflex t) => DOMNode -> m (Dynamic t Int)
example parent = do
  div <- el_ parent "div" mempty
  _ <- el_ div "input" mempty
  _ <- gallery div

  but1 <- el_ parent "button" $ "background" =: "blue"
  but2 <- el_ div "button" $ "background" =: "red"
  clicks <- foldDyn (+) 0 $ 1 <$ leftmost [click but1, click but2]
  pure clicks

gallery :: MonadIO m => DOMNode -> m [DOMNode]
gallery parent = do
  for ["A", "B", "C"] $ \src -> do
    el_ parent "img" $ "src" =: (src <> ".png")

root :: (MonadFix m, MonadIO m, MonadHold t m, Reflex t) => m DOMNode
root = mkRoot $ \(_h,b) -> example b

{- Stubs -}
type Tag = Text
type Children = Seq DOMNode
type Attributes = Map Text Text

data DOMNode = DOMNode Tag (IORef Attributes) (IORef Children)

click :: Reflex t => DOMNode -> Event t ()
click _ = never

el_ :: MonadIO m => DOMNode -> Tag -> Map Text Text -> m DOMNode
el_ (DOMNode _ _ childrenRef) tag' attrs = liftIO $ do
  n <- DOMNode tag' <$> newIORef attrs <*> newIORef mempty
  modifyIORef childrenRef (|> n)
  pure n

mkRoot :: MonadIO m => ((DOMNode, DOMNode) -> m a) -> m DOMNode
mkRoot f = do
  html <- liftIO $ DOMNode "html" <$> newIORef mempty <*> newIORef mempty
  h <- el_ html "head" mempty
  b <- el_ html "body" mempty
  _ <- f (h,b)
  pure html

{- Debug -}
currentTree :: MonadIO m => DOMNode -> m (Tree String)
currentTree (DOMNode tag' attrsRef childrenRef) = liftIO $ do
  attrs <- readIORef attrsRef
  children <- readIORef childrenRef
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

  Node attrs' <$> traverse currentTree (toList children)
