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
import Data.Functor.Identity
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Route

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree

import Prelude hiding (div)

data Const2 a m b = Const2 a

data TTag
  = TTagLeaf
  | TTagBranch

data T (tag :: TTag) (children :: [(* -> *) -> * -> *]) (m :: * -> *) (spine :: *) where
  TLeaf :: m x -> T 'TTagLeaf '[Const2 x] m spine
  TBranch :: spine -> V (x ': xs) T m spine -> T 'TTagBranch (x ': xs) m spine

deriving instance Functor (T tag children m)
deriving instance Foldable (T tag children m)
deriving instance Traversable (T tag children m)

data V (shape :: [(* -> *) -> * -> *]) (f :: k -> [(* -> *) -> * -> *] -> (* -> *) -> * -> *) (m :: * -> *) a where
  VNil :: V '[] f m a
  VCons :: f t xs m a -> V xss f m a -> V (f t xs : xss) f m a

deriving instance (forall tag xs. Functor (f tag xs m)) => Functor (V l f m)
deriving instance (forall tag xs. Foldable (f tag xs m)) => Foldable (V l f m)
deriving instance (forall tag xs. Traversable (f tag xs m)) => Traversable (V l f m)

mapVT :: (forall tag xs. T tag xs m a -> T tag xs m b) -> V shape T m a -> V shape T m b
mapVT f = \case
  VNil -> VNil
  VCons t xs -> VCons (f t) (mapVT f xs)

traverseVT :: Applicative m => (forall tag xs. T tag xs m a -> m (T tag xs n b)) -> V xss T m a -> m (V xss T n b)
traverseVT f = \case
  VNil -> pure VNil
  VCons t v -> VCons <$> f t <*> traverseVT f v

elTree' :: DomBuilder t m => Tree (Text, Map Text Text) -> m (Tree (Element EventResult (DomBuilderSpace m) t))
elTree' (Node (tg, attrs) children) = do
  (n, cs) <- elAttr' tg attrs $ traverse elTree' children
  pure $ Node n cs

elTree :: DomBuilder t m => T tag xs m (Text, Map Text Text) -> m (T tag xs Identity (Element EventResult (DomBuilderSpace m) t))
elTree = \case
  TLeaf x -> TLeaf . Identity <$> x
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


      t0 <- elTree $ TLeaf $ el "div" $ pure (0 :: Int)

      let (TLeaf (Identity zero)) = t0
      text $ T.pack $ show zero

      elTree (TLeaf (el "div" blank)) >>= \case
        TLeaf (Identity ()) -> pure ()

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
        $ VCons (TLeaf (el "img" blank))
        $ VCons (TBranch ("div", mempty) (VCons (TLeaf (el "br" blank)) VNil))
        VNil

      let (TBranch _div (VCons (TLeaf _)
                  (VCons (TBranch __div (VCons _br VNil)) VNil))) = t1

      {-
         warning: [-Woverlapping-patterns]
             Pattern match has inaccessible right hand side
         warning: [-Winaccessible-code]
             • Couldn't match type ‘'[T 'TTagBranch '[],
                                      T 'TTagBranch '[T 'TTagBranch '[]]]’
                              with ‘'[]’

      let (TBranch _div' VNil) = t1
      -}

      {-
         warning: [-Woverlapping-patterns]
             Pattern match has inaccessible right hand side
         warning: [-Winaccessible-code]
            • Couldn't match type ‘'[T 'TTagBranch '[]]’ with ‘'[]’

      let (TBranch _div (VCons (TBranch _img VNil)
                 (VCons (TBranch __div VNil) VNil))) = t1
      -}
      _ <- elTree (TLeaf $ el "br" blank)

      elTree (TLeaf $ el "br" blank) >>= \case
        TLeaf _br -> pure ()

      {-
        • Couldn't match expected type ‘p0’
                      with actual type ‘Element EventResult (DomBuilderSpace m) t’
            ‘p0’ is untouchable
              inside the constraints: 'TTagLeaf ~ 'TTagBranch

      t2 <- elTree (TLeaf $ el "br" blank)
      let TBranch _node _children = t2
     -}

      t3 <- elTree $ TBranch ("div", mempty)
        $ VCons (TLeaf (el "img" blank))
        $ VCons (TLeaf $ el "br" $ pure (3 :: Int))
        $ VNil

      let
        (TBranch _div
         (VCons (TLeaf _img)
          (VCons (TLeaf (Identity three))
           VNil))) = t3

      text $ T.pack $ show $ three

      pure ()
  }
