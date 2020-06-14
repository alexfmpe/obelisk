{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Data.Map (Map)
import Data.Text (Text)
import Data.Tree

import Prelude hiding (div)

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
      return ()



      t <- elTree' $ Node ("div", "style" =: "background:green")
        [ Node ("img", "src" =: static @"obelisk.jpg") []
        ]

      case t of
        Node _div [_img] -> text "lol"
        _ -> error "noes"




      t0 <- elTree $ T ("div", mempty) VNil

      -- no issues
      let (T _div VNil) = t0

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[]’ with ‘T xs : xss’
      let (T _div (VCons _ _)) = t0

      -- (T div VNil) <- elTree $ T ("div", mempty) VNil -- Could not deduce MonadFail

      t1 <- elTree $ T ("div", mempty)
        $ VCons (T ("img", mempty) VNil)
        $ VCons (T ("div", mempty) (VCons (T ("br", mempty) VNil) VNil))
        VNil

      -- no issues
      let (T _div (VCons (T _img VNil)
                  (VCons (T __div (VCons _br VNil)) VNil))) = t1

      -- no issues
      let (T _div (VCons _
                  (VCons (T __div (VCons _br VNil)) _))) = t1

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[T '[], T '[T '[]]]’ with ‘'[]’

      let (T _div' VNil) = t1

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[T '[]]’ with ‘'[]’
      let (T _div (VCons (T _img VNil)
                  (VCons (T __div VNil) VNil))) = t1
      pure ()
  }

elTree' :: DomBuilder t m => Tree (Text, Map Text Text) -> m (Tree (Element EventResult (DomBuilderSpace m) t))
elTree' (Node (tg, attrs) children) = do
  (n, cs) <- elAttr' tg attrs $ traverse elTree' children
  pure $ Node n cs

data T xs a = T a (V xs a)
data V (l :: [* -> *]) a where
  VNil :: V '[] a
  VCons :: T xs a -> V xss a -> V (T xs : xss) a

traverseT :: Monad m => (a -> m b) -> T xs a -> m (T xs b)
traverseT f (T a xs) = do
  T <$> f a <*> traverseV f xs

traverseV :: Monad m => (a -> m b) -> V xs a -> m (V xs b)
traverseV f = traverseVT $ traverseT f

traverseVT :: Applicative m => (forall xs. T xs a -> m (T xs b)) -> V xss a -> m (V xss b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree :: DomBuilder t m => T xs (Text, Map Text Text) -> m (T xs (Element EventResult (DomBuilderSpace m) t))
elTree (T (tg, attrs) xs) = do
  (n, cs) <- elAttr' tg attrs $ traverseVT elTree xs
  pure $ T n cs
