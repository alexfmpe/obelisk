{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Frontend where

import Control.Monad

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Route

import Data.Functor.Const
import Data.Map (Map)
import Data.Text (Text)
import Data.Tree

import Prelude hiding (div)

data T (children :: [* -> *]) (spine :: *) where
  TLeaf :: x -> T '[Const x] spine
  TBranch :: spine -> V children T spine -> T children spine

deriving instance Functor (T children)
deriving instance Foldable (T children)
deriving instance Traversable (T children)

data V (l :: [* -> *]) (f :: [* -> *] -> * -> *) a where
  VNil :: V '[] f a
  VCons :: f xs a -> V xss f a -> V (f xs : xss) f a

deriving instance (forall xs. Functor (f xs)) => Functor (V l f)
deriving instance (forall xs. Foldable (f xs)) => Foldable (V l f)
deriving instance (forall xs. Traversable (f xs)) => Traversable (V l f)

mapVT :: (forall xs. T xs a -> T xs b) -> V shape T a -> V shape T b
mapVT f = \case
  VNil -> VNil
  VCons t xs -> VCons (f t) (mapVT f xs)

traverseVT :: Applicative m => (forall xs. T xs a -> m (T xs b)) -> V xss T a -> m (V xss T b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree' :: DomBuilder t m => Tree (Text, Map Text Text) -> m (Tree (Element EventResult (DomBuilderSpace m) t))
elTree' (Node (tg, attrs) children) = do
  (n, cs) <- elAttr' tg attrs $ traverse elTree' children
  pure $ Node n cs

elTree :: DomBuilder t m => T xs (Text, Map Text Text) -> m (T xs (Element EventResult (DomBuilderSpace m) t))
elTree = \case
  TLeaf x -> pure $ TLeaf x
  TBranch (tg, attrs) xs -> do
    (n, cs) <- elAttr' tg attrs $ traverseVT elTree xs
    pure $ TBranch n cs

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do



      t <- elTree' $ Node ("div", "style" =: "background:green")
        [ Node ("img", "src" =: static @"obelisk.jpg") []
        ]

      case t of
        Node _div [_img] -> text "lol"
        _ -> error "noes"



      t0 <- elTree $ TBranch ("div", mempty) VNil

      -- no issues
      let (TBranch _div VNil) = t0

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[]’ with ‘T xs : xss’
      let (TBranch _div (VCons _ _)) = t0

      -- • Could not deduce: x ~ p0
      -- let (TLeaf x) = t0

      -- • Could not deduce MonadFail
      -- (TBranch div VNil) <- elTree $ TBranch ("div", mempty) VNil


      t1 <- elTree $ TBranch ("div", mempty)
        $ VCons (TBranch ("img", mempty) VNil)
        $ VCons (TBranch ("div", mempty) (VCons (TBranch ("br", mempty) VNil) VNil))
        VNil

      -- no issues
      let (TBranch _div (VCons (TBranch _img VNil)
                  (VCons (TBranch __div (VCons _br VNil)) VNil))) = t1

      -- no issues
      let (TBranch _div (VCons _
                  (VCons (TBranch __div (VCons _br VNil)) _))) = t1

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[T '[], T '[T '[]]]’ with ‘'[]’

      let (TBranch _div' VNil) = t1

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[T '[]]’ with ‘'[]’
      let (TBranch _div (VCons (TBranch _img VNil)
                  (VCons (TBranch __div VNil) VNil))) = t1

      pure ()
  }
