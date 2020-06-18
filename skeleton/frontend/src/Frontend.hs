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

{-# OPTIONS_GHC -Werror=inaccessible-code #-}
{-# OPTIONS_GHC -Werror=overlapping-patterns #-}

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

data TTag
  = TTagLeaf
  | TTagBranch

data T (tag :: TTag) (children :: [* -> *]) (spine :: *) where
  TLeaf :: x -> T 'TTagLeaf '[Const x] spine
  TBranch :: spine -> V children T spine -> T 'TTagBranch children spine

deriving instance Functor (T tag children)
deriving instance Foldable (T tag children)
deriving instance Traversable (T tag children)

data V (l :: [* -> *]) (f :: k -> [* -> *] -> * -> *) a where
  VNil :: V '[] f a
  VCons :: f t xs a -> V xss f a -> V (f t xs : xss) f a

deriving instance (forall tag xs. Functor (f tag xs)) => Functor (V l f)
deriving instance (forall tag xs. Foldable (f tag xs)) => Foldable (V l f)
deriving instance (forall tag xs. Traversable (f tag xs)) => Traversable (V l f)

mapVT :: (forall tag xs. T tag xs a -> T tag xs b) -> V shape T a -> V shape T b
mapVT f = \case
  VNil -> VNil
  VCons t xs -> VCons (f t) (mapVT f xs)

traverseVT :: Applicative m => (forall tag xs. T tag xs a -> m (T tag xs b)) -> V xss T a -> m (V xss T b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree' :: DomBuilder t m => Tree (Text, Map Text Text) -> m (Tree (Element EventResult (DomBuilderSpace m) t))
elTree' (Node (tg, attrs) children) = do
  (n, cs) <- elAttr' tg attrs $ traverse elTree' children
  pure $ Node n cs

elTree :: DomBuilder t m => T tag xs (Text, Map Text Text) -> m (T tag xs (Element EventResult (DomBuilderSpace m) t))
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

      let (TBranch _div VNil) = t0

      elTree (TBranch ("div", mempty) VNil) >>= \case
        TBranch _div VNil -> pure ()

      -- • Could not deduce MonadFail
      -- (TBranch div VNil) <- elTree $ TBranch ("div", mempty) VNil

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[]’ with ‘T xs : xss’
      -- let (TBranch _div (VCons _ _)) = t0

      -- • Could not deduce: x ~ p0
      -- let (TLeaf x) = t0


      t1 <- elTree $ TBranch ("div", mempty)
        $ VCons (TBranch ("img", mempty) VNil)
        $ VCons (TBranch ("div", mempty) (VCons (TBranch ("br", mempty) VNil) VNil))
        VNil

      let (TBranch _div (VCons (TBranch _img VNil)
                  (VCons (TBranch __div (VCons _br VNil)) VNil))) = t1

      let (TBranch _div (VCons _
                  (VCons (TBranch __div (VCons _br VNil)) _))) = t1

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[T '[], T '[T '[]]]’ with ‘'[]’
      -- let (TBranch _div' VNil) = t1

      -- warning: [-Woverlapping-patterns]
      --     Pattern match has inaccessible right hand side
      -- warning: [-Winaccessible-code]
      --     Couldn't match type ‘'[T '[]]’ with ‘'[]’
      -- let (TBranch _div (VCons (TBranch _img VNil)
      --             (VCons (TBranch __div VNil) VNil))) = t1

      elTree (TLeaf $ el "br" blank) >>= \case
        TLeaf br -> br

      {-
      -- warning: [-Winaccessible-code, -Werror=inaccessible-code]
      --     • Couldn't match type ‘'TTagLeaf’ with ‘'TTagBranch’
      elTree (TLeaf $ el "br" blank) >>= \case
        TLeaf br -> br
        TBranch _ _ -> pure ()
      -}

      t3 <- elTree $ TBranch ("div", mempty)
        $ VCons (TLeaf (0 :: Int))
        $ VCons (TBranch ("img", mempty) VNil)
        $ VCons (TLeaf $ el "br" blank)
        $ VNil

      let
        (TBranch _div
         (VCons (TLeaf _zero)
          (VCons (TBranch _img VNil)
           (VCons (TLeaf br')
            VNil)))) = t3
      br'

      pure ()
  }
