{-# LANGUAGE ApplicativeDo #-}
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
import Data.Functor.Identity
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
check overlaps in \/
decoding
isos
interval length
incremental
-}

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

  Enum    :: (Enum a, Finite a) => Text -> Format a Tag --TODO: keep text description?

--  Symbol :: (Show a, Show b) => a -> Vector b -> Format a b --TODO: Show?
--  Symbol :: a -> Format a a
-- TODO: dynamic length? how to invert compose of vector?
--  Vector :: TargetWord -> Format TargetWord b -> Format a b -> Format (Vector a) b


infixr 6 \/
(\/) :: Applicative check => EncoderK check Format a c -> EncoderK check Format b c -> EncoderK check Format (Either a b) c
ea \/ eb = Encoder $ do
  fa <- unEncoder ea
  fb <- unEncoder eb
  --TODO: check overlaps
  pure $ Sum fa fb

infixr 7 /\
(/\) :: Applicative check => EncoderK check Format a c -> EncoderK check Format b c -> EncoderK check Format (a,b) c
ea /\ eb = Encoder $ do
  fa <- unEncoder ea
  fb <- unEncoder eb
  pure $ Product fa fb

instance Semigroupoid Format where
  o = Compose
instance Category Format where
  (.) = o
  id = Id

enum :: (Applicative check, Enum a, Finite a) => Text -> EncoderK check Format a Tag
enum a = unsafeMkEncoder $ Enum a

length :: forall a b. Word -> Word -> EncoderK Identity Format a b -> Word
length tagBits bitsInB (Encoder (Identity fmt)) = go fmt
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
describe :: EncoderK Identity Format a b -> String
describe (Encoder (Identity fmt)) = drawTree $ go fmt
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

explain :: EncoderK Identity Format a Word8 -> String
explain format = unlines
  [ "Binary encoding has a length of " <> show (length 8 8 format) <> " bytes. Format is as follows:"
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
{-
runParser :: Format a b -> (b -> Either Text a)
runParser
-}

first :: Monad parse => parse a -> (a -> parse b) -> parse (a, b)
first fa fb = do
  a <- fa
  b <- fb a
  pure (a,b)

x :: Applicative parse => parse Word
x = pure 0

xx :: Monad parse => parse (Word, (Word, Word))
xx = first x $ \_ -> first x $ \_ -> x

--firstCont :: ContT parse a
--firstCont =
-- :: MonadState Word parse => ? -> parse a -> parse b -> parse (Either a b)

toEncoderBytes
  :: (Functor check, MonadState Word parse, MonadError Text parse)
  => EncoderK check Format a Word8 -> EncoderK check (EncoderImpl parse) a (Vector Word8)
toEncoderBytes = unsafeLowerCategory $ toEncoderImpl 8 8

toEncoderImpl
  :: forall parse a b. (MonadState Word parse, MonadError Text parse)
  => Word -> Word -> Format a b -> EncoderImpl parse a (Vector b)
toEncoderImpl tagBits bitsInB = go
  where
    go :: Format a' b' -> EncoderImpl parse a' (Vector b')
    go = \case
      --TODO: bug: padding?
      Sum fx fy ->
        let
          ix = go fx
          iy = go fy
        in EncoderImpl
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

      Zero -> EncoderImpl
        { _encoderImpl_encode = absurd
        , _encoderImpl_decode = const $ throwError "Decoding to void always fails"
        }

      Product fx fy ->
        let
          ix = go fx
          iy = go fy
        in EncoderImpl
          { _encoderImpl_encode = \(x, y) -> _encoderImpl_encode ix x <> _encoderImpl_encode iy y
          , _encoderImpl_decode = \bs -> do
              c0 <- get
              x <- _encoderImpl_decode ix bs
              c1 <- get
              y <- _encoderImpl_decode iy $ Vector.drop (fromIntegral $ c1 - c0) bs
              pure (x,y)
          }

      One -> EncoderImpl
        { _encoderImpl_encode = \() -> mempty
        , _encoderImpl_decode = const $ pure ()
        }

      Compose b2c a2b ->
        let
          ib2c = go b2c
          ia2b = go a2b
        in EncoderImpl
          { _encoderImpl_encode = _encoderImpl_encode ia2b >=> _encoderImpl_encode ib2c
          , _encoderImpl_decode = \bs -> do
              let
                chunks :: Word -> Vector x -> parse (Vector (Vector x))
                chunks n = fmap Vector.fromList . f
                  where
                    f v = case Vector.length v of
                      0 -> pure mempty
                      l | l < fromIntegral n -> throwError "Ran out of bytes while decoding chunk in Compose"
                      _ -> let (a,b) = Vector.splitAt (fromIntegral n) v in (a :) <$> f b
              cs <- chunks (length tagBits bitsInB $ unsafeMkEncoder b2c) bs --TODO: superfluous mkEncoder
              for cs (_encoderImpl_decode ib2c) >>= _encoderImpl_decode ia2b
          }

      Id -> EncoderImpl
        { _encoderImpl_encode = Vector.singleton
        , _encoderImpl_decode = \bs -> case bs Vector.!? 0 of
            Nothing -> throwError "Ran out of bytes while decoding Id"
            Just a -> do
              modify succ
              pure a
        }

      Enum _ -> EncoderImpl
        { _encoderImpl_encode = Vector.singleton . fromIntegral . fromEnum
        , _encoderImpl_decode = \bs -> case bs Vector.!? 0 of
            Nothing -> throwError "Ran out of bytes while decoding Enum"
            Just b -> do
              modify succ
              pure $ toEnum $ fromIntegral b
        }

{-
--  Isomorphism :: Isomorphism a b -> Format a b
data Isomorphism a b where
  Iso_Id   :: Isomorphism a a
  Iso_Swap :: Isomorphism (a,b) (b,a)

--ex1 :: Format Word8 Word8
--ex1 = Isomorphism Iso_Id
-}
