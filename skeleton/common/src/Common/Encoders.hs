{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Common.Encoders where

import Control.Monad
import Control.Monad.Writer
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.HexString
import Data.Word

type SomeData = () :. Word8 :. Word8 :. Either () ()

test :: IO ()
test = do
  let
    f :: Format SomeData
    f =   Format_Magic "XYZ"
      :*: Format_Byte
      :*: Format_Byte
      :*: (Format_Magic "!" :+: Format_Magic "?")

    a :: SomeData
    a =  ()
      :. 0x61
      :. 0x62
      :. Left ()

    b = encodeHaskBinary f a
    c = decodeHaskBinary f b
  print a
  print b
  print $ fromBytes $ ByteString.toStrict b
  print c

type (:.) = (,)
infixr 5 :.
pattern (:.) :: a -> b -> a :. b
pattern a :. b = (a, b)

infixr 7 :*:
pattern (:*:) :: Format a -> Format b -> Format (a,b)
pattern a :*: b = Format_Product a b

infixr 6 :+:
pattern (:+:) :: Format a -> Format b -> Format (Either a b)
pattern a :+: b = Format_Sum a b

data Format a where
  Format_Magic :: ByteString -> Format ()
  Format_Byte :: Format Word8
  Format_Product :: Format a -> Format b -> Format (a,b)
  Format_Sum :: Format a -> Format b -> Format (Either a b)

epsilon :: Format ()
epsilon = Format_Magic ""

unsafeMkEncoder :: Applicative check => EncoderImpl decode encode decoded encoded -> Encoder check decode encode decoded encoded
unsafeMkEncoder impl = Encoder (pure impl)

newtype Encoder check decode encode decoded encoded =
  Encoder { unEncoder :: check (EncoderImpl decode encode decoded encoded) }

data EncoderImpl decode encode decoded encoded = EncoderImpl
  { _encoderImpl_decode :: !(decode encoded decoded)
  , _encoderImpl_encode :: !(encode decoded encoded)
  }

mkHaskBinaryEncoder :: Applicative check => Format a -> Encoder check (->) (->) a ByteString
mkHaskBinaryEncoder f = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = decodeHaskBinary f
  , _encoderImpl_encode = encodeHaskBinary f
  }

encodeHaskBinary :: Format a -> (a -> ByteString)
encodeHaskBinary f = Binary.runPut . go f
  where
    go :: Format a -> (a -> Binary.Put)
    go = \case
      Format_Magic bs -> \() -> Binary.putByteString $ ByteString.toStrict bs
      Format_Byte -> Binary.put
      Format_Product fa fb -> \(a,b) -> go fa a *> go fb b
      Format_Sum fa fb -> \case
        Left a -> Binary.putWord8 0 *> go fa a
        Right b -> Binary.putWord8 1 *> go fb b

decodeHaskBinary :: Format a -> (ByteString -> a)
decodeHaskBinary = Binary.runGet . go
  where
    go :: Format a -> Binary.Get a
    go = \case
      Format_Magic bs -> do
        bs' <- Binary.getByteString (fromIntegral $ ByteString.length bs)
        when (bs' /= ByteString.toStrict bs) $ error "derp"
      Format_Byte -> Binary.get
      Format_Product fa fb -> pure (,) <*> go fa <*> go fb
      Format_Sum fa fb -> Binary.getWord8 >>= \case
        0 -> Left <$> go fa
        1 -> Right <$> go fb
        _ -> error "derp"
