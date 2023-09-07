{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Obelisk.Route.Inv (Inv(Inv)) where

import Prelude hiding (Functor(..), id, fst, snd, (.))
import Data.Bitraversable
import Data.Semigroupoid
import Control.Arrow (Kleisli(..))
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Category.Monoidal
import Control.Categorical.Functor
import Control.Categorical.Bifunctor

k :: Applicative m => (a -> b) -> Kleisli m a b
k f = Kleisli $ pure . f

instance (Monad m, Traversable f) => Functor f (Kleisli m) (Kleisli m) where
  fmap f = Kleisli $ traverse (runKleisli f)

instance (Monad m, Bitraversable p) => PFunctor p (Kleisli m) (Kleisli m) where
  first f = bimap f id

instance (Monad m, Bitraversable p) => QFunctor p (Kleisli m) (Kleisli m) where
  second g = bimap id g

instance (Monad m, Bitraversable p) => Bifunctor p (Kleisli m) (Kleisli m) (Kleisli m) where
  bimap f g = Kleisli $ bitraverse (runKleisli f) (runKleisli g)

instance (Monad m, Bitraversable p, Associative (->) p) => Associative (Kleisli m) p where
  associate = k associate
  disassociate = k disassociate

instance (Monad m, Bitraversable p, Monoidal (->) p) => Monoidal (Kleisli m) p where
  type Id (Kleisli m) p = Id (->) p
  idl = k idl
  idr = k idr
  coidl = k coidl
  coidr = k coidr

instance (Monad m, Bitraversable p, Braided (->) p) => Braided (Kleisli m) p where
  braid = k braid

instance (Monad m, Bitraversable p, Symmetric (->) p) => Symmetric (Kleisli m) p where

instance Monad m => Cartesian (Kleisli m) where
  type Product (Kleisli m) = (,)
  fst = k fst
  snd = k snd
  diag = k $ \a -> (a,a)

instance Monad m => CoCartesian (Kleisli m) where
  type Sum (Kleisli m) = Either
  inl = k inl
  inr = k inr
  codiag = k $ either id id

-- h cat homomorphism
-- fw injective
-- h injective
-- bw . h fw = id

--data Inv fw bw a b = (Semigroupoid fw, Category fw, Semigroupoid bw, Category bw) => Inv
data Inv fw bw a b = Inv
  { _fw :: fw a b
  , _bw :: bw b a
  }

instance (Semigroupoid fw, Semigroupoid bw) => Semigroupoid (Inv fw bw) where
  Inv fy by `o` Inv fx bx = Inv (fy `o` fx) (bx `o` by)

{-
Associativity
  z . (y . x) = (z . y) . x

  Inv fz bz . (Inv fy by . Inv fx bx) = (Inv fz bz . Inv fy by) . Inv fx bx
= Inv fz bz . Inv (fy . fx) (bx . by) = Inv (fz . fy) (by . bz). Inv fx bx
= Inv (fz . (fy . fx)) ((bx . by) . bz) = Inv ((fz . fy) . fx) (bx . (by . bz))
= Inv (fz . fy . fx) (bx . by . bz) = Inv (fz . fy . fx) (bx . by . bz)

Invariant
  bw . h fw = id

  (b1 . b2) . h (f2 . f1)
= b1 . b2 . h f2 . h f1
= b1 . id . h f1
= b1 . h f1
= id

-}


instance (Semigroupoid (Inv fw bw), Category fw, Category bw) => Category (Inv fw bw) where
  id = Inv id id
  (.) = o

{-
Identity
  id . x = x = x . id

  Inv id id . Inv f b = Inv (id . f) (b . id) = Inv f b = Inv (f . id) (id . b) = Inv f b . Inv id id
-}



instance (Semigroupoid (Inv fw bw), Functor f fw fw, Functor f bw bw) => Functor f (Inv fw bw) (Inv fw bw) where
  fmap (Inv f b) = Inv (fmap f) (fmap b)

{-
  fmap (Inv id id)
= Inv (fmap id) (fmap id)
= Inv id id
= id
-}

{-
  first :: r a a' -> t (a b) (a' b)
  second :: s a a' -> s (a b) (a b')
  bimap :: r a a' -> s b b' -> t (p a b) (p a' b')

Identity
  first id = id
  second id = id
  bimap id id = id

Composition
  first g . first f = first (g . f)
  second g . second f = second (g . f)
  bimap f1 f2 . bimap g1 g2 = bimap (f1 . g1) (f2 . g2)

Defaults

  first f
= id . first f
= second id . first f
= bimap f id

  second g
= second g . id
= second g . first id
= bimap id g

  bimap f g = second g . first f

  bimap f g
= bimap (id . f) (g . id)
= bimap id g . bimap f id
= second g . first f

  bimap f g
= bimap (f . id) (id . g)
= bimap f id . bimap id g
= first f . second g

Proofs

  first g . first f
= bimap g id . bimap f id
= bimap (g . f) (id . id)
= first (g . f)

  second g . second f
= bimap id g . bimap id f
= bimap (id . id) (g . f)
= second (g . f)

  bimap id id
= second id . first id
= id . id
= id(g . f)

  bimap f1 f2 . bimap g1 g2
= second f2 . first f1 . first g1 . second g2
= second f2 . first (f1 . g1) . second g2
= second f2 . second g2 . first (f1 . g1)
= second (f2 . g2) . first (f1 . g1)
= bimap (f2 . g2) (f1 . g1)
-}

{-
instance (PFunctor p fw fw, PFunctor p bw bw, Category (Inv fw bw)) => PFunctor p (Inv fw bw) (Inv fw bw) where
  first :: Inv fw bw a b -> Inv fw bw (p a c) (p b c)
  first (Inv f b) = Inv (first f) (first b)
-}

instance (Semigroupoid (Inv fw bw), PFunctor p fw fw, PFunctor p bw bw) => PFunctor p (Inv fw bw) (Inv fw bw) where
  first :: Inv fw bw a a' -> Inv fw bw (p a b) (p a' b)
  first (Inv f b) = Inv (first f) (first b)

instance (Semigroupoid (Inv fw bw), QFunctor p fw fw, QFunctor p bw bw) => QFunctor p (Inv fw bw) (Inv fw bw) where
  second :: Inv fw bw b b' -> Inv fw bw (p a b) (p a b')
  second (Inv f b) = Inv (second f) (second b)

instance (Semigroupoid (Inv fw bw), Bifunctor p fw fw fw, Bifunctor p bw bw bw) => Bifunctor p (Inv fw bw) (Inv fw bw) (Inv fw bw) where
  bimap f g = second g . first f

{-
  first id
= first (Inv id id)
= Inv (first id) (first id)
= Inv id id
= id

  first y . first x
= Inv (first fy) (first by) . Inv (first fx) (first bx)
= Inv (first fy . first fx) (first bx . first by)
= Inv (first (fy . fx)) (first (bx . by))
= first (Inv (fy . fx) (bx . by))
= first (Inv fy by . Inv fx bx)
= first (y . x)



Invariant
  first b . h (first f)
= ???
= first b . first (h f)
= first (b . h f)
= first id
  id

-}

instance (Semigroupoid (Inv fw bw), Associative fw p, Associative bw p) => Associative (Inv fw bw) p where
  associate :: Inv fw bw (p (p a b) c) (p a (p b c))
  associate = Inv associate disassociate
  disassociate = Inv disassociate associate

--bimap id associate . associate . bimap associate id = associate . associate
--bimap disassociate id . disassociate . bimap id disassociate = disassociate . disassociate

instance (Semigroupoid (Inv fw bw), Symmetric fw p, Symmetric bw p) => Braided (Inv fw bw) p where
  braid = Inv braid braid

{-
  braid . h braid
= ???
= id
-}

instance (Semigroupoid (Inv fw bw), Symmetric fw p, Symmetric bw p) => Symmetric (Inv fw bw) p where

{-
  swap . swap
= braid . braid
= Inv braid braid . Inv braid braid
= Inv swap swap . Inv swap swap
= Inv (swap . swap) (swap . swap)
= Inv id id
= id
-}

instance (Semigroupoid (Inv fw bw), Monoidal fw p, Monoidal bw p, Id fw p ~ Id bw p) => Monoidal (Inv fw bw) p where
 type Id (Inv fw bw) p = Id fw p
 idl = Inv idl coidl
 idr = Inv idr coidr
 coidl = Inv coidl idl
 coidr = Inv coidr idr

{-
-- first idr = second idl . associate
-- second idl = first idr . associate
-- first idr = disassociate . second idl
-- second idl = disassociate . first idr

  idl . coidl
= Inv idl coidl . Inv coidl idl
= Inv (idl . coidl) (coidl . idl)
= Inv id id
= id

  idl . h coidl
= ???
= id
-}
