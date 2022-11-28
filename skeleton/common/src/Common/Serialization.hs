{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Common.Serialization where

import Prelude hiding (length, id, (.))

import Control.Category
import Control.Lens (Iso', iso)
import Control.Monad.Except
import Control.Monad.State
import Data.Bool
import Data.Semigroupoid
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable
import Data.Tree
import Data.Universe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void
import Data.Word
import Numeric.Natural

import Common.Route
import Obelisk.Route

{-
-- TODO
checks
decoding
interval length
incremental
-}

infixr 6 \/
(\/) :: Format a c -> Format b c -> Format (Either a b) c
(\/) = Sum

infixr 7 /\
(/\) :: Format a c -> Format b c -> Format (a,b) c
(/\) = Product

-- TODO: generalize?
-- TODO: Enum class uses Int
type Tag = Word8

type TargetWord = Word8

-- (a -> [b], [b] -> Maybe a)
--   Sum     :: Format Tag c -> Format a c -> Format b c -> Format (Either a b) c
data Format a b where
  Sum     :: Format a c -> Format b c -> Format (Either a b) c
  Zero    :: Format Void c

  Product :: Format a c -> Format b c -> Format (a,b) c
  One     :: Format () c

  Compose :: Format b c -> Format a b -> Format a c
  Id      :: Format a a

  Enum    :: (Enum a, Finite a) => Text -> Format a Tag
--  Symbol :: (Show a, Show b) => a -> Vector b -> Format a b --TODO: Show?
--  Symbol :: a -> Format a a
-- TODO: dynamic length? how to invert compose of vector?
--  Vector :: TargetWord -> Format TargetWord b -> Format a b -> Format (Vector a) b

instance Semigroupoid Format where
  o = Compose
instance Category Format where
  (.) = o
  id = Id

length :: forall a b. Word -> Word -> Format a b -> Word
length tagBits bitsInB = go
  where
--    targetWords :: Word -> Word -> Word
--    targetWords count targetWordCount = 1 + ((count - 1) `div` targetWordCount)

    s :: Natural -> Word
    s x = ceiling @Double $ log (fromIntegral x) / log 2 / fromIntegral bitsInB --TODO: double-check this

    go :: forall x y. Format x y -> Word
    go = \case
      Sum a b -> 1 + ((tagBits - 1) `div` bitsInB) + max (go a) (go b) --TODO: fix
      Zero -> 0

      Product a b -> go a + go b
      One -> 0

      Compose b2c a2b -> go b2c * go a2b
      Id -> 1

      Enum _ -> s $ unTagged $ cardinality @x --TODO: isn't this slow?

--  Symbol _ bs -> fromIntegral $ Vector.length bs
--  Vector len l2b a2b -> length l2b + fromIntegral len * length a2b

--TODO: collapse nested pairs?
describe :: Format a b -> String
describe = drawTree . go
  where
    go :: Format a b -> Tree String
    go = \case
      Sum a b -> Node "One of" [go a, go b] --TODO: describe tag
      Zero -> Node "Void" []

      Product a b -> Node "All of" [go a, go b]
      One -> Node "Unit" []

      Compose b2c a2b -> Node "Chain of" [go b2c, go a2b]
      Id -> Node "Id" []

      Enum description -> Node ("Enum: " <> Text.unpack description) []
--      Symbol a bs -> Node (show a <> " encoded as " <> show bs) []

explain :: Format a Word8 -> String
explain format = unlines [ "Binary encoding has a length of " <> show (length 8 8 format) <> " bytes. Format is as follows:"
                         , describe format
                         ]

{-
      --TODO: bug: padding?
      Sum fx fy -> unsafeEncoder $ do
        it <- unEncoder $ go ft
        ix <- unEncoder $ go fx
        iy <- unEncoder $ go fy
        pure $ EncoderImpl
          { _encoderImpl_encode = \case
              Left  x -> _encoderImpl_encode it 0 <> _encoderImpl_encode ix x
              Right y -> _encoderImpl_encode it 1 <> _encoderImpl_encode iy y
          , _encoderImpl_decode = \bs -> do
              _encoderImpl_decode it bs >>= \case
                0 -> fmap Left  $ _encoderImpl_decode ix $ Vector.drop 1 bs
                1 -> fmap Right $ _encoderImpl_decode iy $ Vector.drop 1 bs
                _ -> throwError "Invalid tag"
          }
-}

toEncoderImplBytes
  :: (MonadError Text check, MonadState Word parse, MonadError Text parse)
  => Format a Word8 -> EncoderK check (EncoderImpl parse) a (Vector Word8)
toEncoderImplBytes = toEncoderImpl 8 8

toEncoderImpl
  :: forall check parse a b. (MonadError Text check, MonadState Word parse, MonadError Text parse)
  => Word -> Word -> Format a b -> EncoderK check (EncoderImpl parse) a (Vector b)
toEncoderImpl tagBits bitsInB = go
  where
    go :: Format a' b' -> EncoderK check (EncoderImpl parse) a' (Vector b')
    go = \case
      --TODO: bug: padding?
      Sum fx fy -> unsafeEncoder $ do
        ix <- unEncoder $ go fx
        iy <- unEncoder $ go fy
        pure $ EncoderImpl
          { _encoderImpl_encode = \case
              Left  x -> _encoderImpl_encode ix x
              Right y -> _encoderImpl_encode iy y
          , _encoderImpl_decode = \bs ->
              let
                l = Left  <$> _encoderImpl_decode ix bs
                r = Right <$> _encoderImpl_decode iy bs
              in
                catchError l (const r)
          }

      Zero -> unsafeMkEncoder $ EncoderImpl
        { _encoderImpl_encode = absurd
        , _encoderImpl_decode = const $ throwError "Decoding to void always fails"
        }

      Product fx fy -> unsafeEncoder $ do
        ix <- unEncoder $ go fx
        iy <- unEncoder $ go fy
        pure $ EncoderImpl
          { _encoderImpl_encode = \(x, y) -> _encoderImpl_encode ix x <> _encoderImpl_encode iy y
          , _encoderImpl_decode = \bs -> do
              c0 <- get
              x <- _encoderImpl_decode ix bs
              c1 <- get
              y <- _encoderImpl_decode iy $ Vector.drop (fromIntegral $ c1 - c0) bs
              pure (x,y)
          }

      One -> unsafeMkEncoder $ EncoderImpl
        { _encoderImpl_encode = \() -> mempty
        , _encoderImpl_decode = const $ pure ()
        }

      Compose b2c a2b -> unsafeEncoder $ do
        ib2c <- unEncoder $ go b2c
        ia2b <- unEncoder $ go a2b
        pure $ EncoderImpl
          { _encoderImpl_encode = _encoderImpl_encode ia2b >=> _encoderImpl_encode ib2c
          , _encoderImpl_decode = \bs -> do
              let
                chunks :: Word -> Vector x -> parse (Vector (Vector x))
                chunks n = fmap Vector.fromList . go
                  where
                    go v = case Vector.length v of
                      0 -> pure mempty
                      l | l < fromIntegral n -> throwError "Ran out of bytes while decoding chunk in Compose"
                      _ -> let (a,b) = Vector.splitAt (fromIntegral n) v in (a :) <$> go b
              cs <- chunks (length tagBits bitsInB b2c) bs
              for cs (_encoderImpl_decode ib2c) >>= _encoderImpl_decode ia2b
          }

      Id -> unsafeMkEncoder $ EncoderImpl
        { _encoderImpl_encode = Vector.singleton
        , _encoderImpl_decode = \bs -> case bs Vector.!? 0 of
            Nothing -> throwError "Ran out of bytes while decoding Id"
            Just a -> do
              modify succ
              pure a
        }

      Enum _ -> unsafeMkEncoder $ EncoderImpl
        { _encoderImpl_encode = Vector.singleton . fromIntegral . fromEnum
        , _encoderImpl_decode = \bs -> case bs Vector.!? 0 of
            Nothing -> throwError "Ran out of bytes while decoding Enum"
            Just b -> do
              modify succ
              pure $ toEnum $ fromIntegral b
        }

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving anyclass (Finite, Universe)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving anyclass (Finite, Universe)

type Card = (Rank, Suit)
type Hand = (Card, (Card, Card))

suit :: Format Suit Word8
suit = Enum "Suit"

rank :: Format Rank Word8
rank = Enum "Rank"

card :: Format Card Word8
card = rank /\ suit

ex1 :: Format Hand Word8
ex1 = card /\ card /\ card

{-
--  Isomorphism :: Isomorphism a b -> Format a b
data Isomorphism a b where
  Iso_Id   :: Isomorphism a a
  Iso_Swap :: Isomorphism (a,b) (b,a)

--ex1 :: Format Word8 Word8
--ex1 = Isomorphism Iso_Id
-}
