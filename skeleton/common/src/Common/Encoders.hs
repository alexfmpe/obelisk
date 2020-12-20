{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Common.Encoders where

import Control.Arrow
import Control.Category
--import qualified Control.Categorical.Functor as Cat
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Int
import Data.List
import Data.HexString
import Data.Semigroupoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (id, (.))
type SomeData = Either () () :. Word8 :. Either () Word8 :. [Word8]

tshow :: Show a => a -> Text
tshow = T.pack . show

test :: IO ()
test = do
  let
    f :: Format SomeData
    f =   (Format_Magic "V1" :+: Format_Magic "V2")
      :*: Format_Byte
      :*: optional Format_Byte
      :*: Format_Replicate Format_Byte Format_Byte

    a :: SomeData
    a =  Left ()
      :. 0x61
      :. Right 0x62
      :. [0x63, 0x64, 0x65]

    enc = either (error . T.unpack) id $ checkEncoder $ haskFormat f

    b = encodeHask enc a
    c = decodeHask enc `runParse` b
  print a
  print b
  print $ fromBytes $ ByteString.toStrict b
  print c

  putStrLn ""
--  putStrLn $ T.unpack $ decideSolidity f
--  putStrLn $ T.unpack $ decodeRust f


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

data HList (f :: * -> *) (types :: [*]) where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

data Labeled a = Labeled Text a
  deriving (Eq, Ord, Show, Generic)

type Fields = HList Labeled

data Format a where
  Format_Magic :: ByteString -> Format ()
  Format_Byte :: Format Word8
  Format_Product :: Format a -> Format b -> Format (a,b)
  Format_Sum :: Format a -> Format b -> Format (Either a b)
  Format_Replicate :: Format Word8 -> Format a -> Format [a]
--  Format_Struct :: HList (Compose Labeled Format) types -> Format (HList Format types)

field :: Text -> Format a -> HList (Compose Labeled Format) as -> HList (Compose Labeled Format) (a ': as)
field lbl f = HCons (Compose (Labeled lbl f))

struct :: HList f '[]
struct = HNil

optional :: Format a -> Format (Either () a)
optional f = Format_Sum epsilon f

epsilon :: Format ()
epsilon = Format_Magic ""

newtype Encoder check spec decoded encoded =
  Encoder { unEncoder :: check (spec decoded encoded) }

data EncoderImpl decode encode decoded encoded = EncoderImpl
  { _encoderImpl_decode :: !(decode encoded decoded)
  , _encoderImpl_encode :: !(encode decoded encoded)
  }

checkEncoder
  :: Functor check
  => Encoder check spec decoded encoded
  -> check (Encoder Identity spec decoded encoded)
checkEncoder = fmap (Encoder . Identity) . unEncoder

hoistSpec
  :: Functor check
  => (forall dec enc. spec dec enc -> spec' dec enc)
  -> Encoder check spec  decoded encoded
  -> Encoder check spec' decoded encoded
hoistSpec f = Encoder . fmap f . unEncoder

decode :: Encoder Identity (EncoderImpl decode encode) decoded encoded -> decode encoded decoded
decode = _encoderImpl_decode . runIdentity . unEncoder

encode :: Encoder Identity (EncoderImpl decode encode) decoded encoded -> encode decoded encoded
encode = _encoderImpl_encode . runIdentity . unEncoder

encodeHask :: Encoder Identity (EncoderImpl DecodeViaGet EncodeViaPut) a ByteString -> (->) a ByteString
encodeHask enc = Binary.runPut . encodeToPut (encode enc)

decodeHask :: Encoder Identity (EncoderImpl DecodeViaGet EncodeViaPut) a ByteString -> Parse ByteString a
decodeHask enc = case decode enc of
  Categoryish_ImplicitSource bs2a m -> bs2a m

instance (Category decode, Category encode) => Category (EncoderImpl decode encode) where
  id = EncoderImpl id id
  f . g = EncoderImpl
    (_encoderImpl_decode f >>> _encoderImpl_decode g)
    (_encoderImpl_encode f <<< _encoderImpl_encode g)

data Implicitness
  = ImplicitSource
  | ImplicitTarget

data Categoryish (x :: Implicitness) f c a b where
  Categoryish_ImplicitSource :: (f b -> c a b) -> f b -> Categoryish 'ImplicitSource f c a b
  Categoryish_ImplicitTarget :: (f a -> c a b) -> f a -> Categoryish 'ImplicitTarget f c a b

newtype Const2 a b c = Const2 { unConst2 :: a }

formatCategoryish :: Format a -> Categoryish 'ImplicitTarget Format (Const2 (Format a)) a ByteString
formatCategoryish = Categoryish_ImplicitTarget (\f -> Const2 f)

explicit :: Categoryish x m c a b -> c a b
explicit = \case
  Categoryish_ImplicitSource a2x mb -> a2x mb
  Categoryish_ImplicitTarget x2b ma -> x2b ma

instance Semigroupoid c => Semigroupoid (Categoryish 'ImplicitSource m c) where
  Categoryish_ImplicitSource b2c mc `o` a2b = Categoryish_ImplicitSource (\m -> b2c m `o` explicit a2b) mc

instance Semigroupoid c => Semigroupoid (Categoryish 'ImplicitTarget m c) where
  b2c `o` Categoryish_ImplicitTarget a2b ma = Categoryish_ImplicitTarget (\m -> explicit b2c `o` a2b m) ma

type DecodeViaGet = Categoryish 'ImplicitSource Binary.Get      Parse
type EncodeViaPut = Categoryish 'ImplicitTarget (Op Binary.Put) (->)

haskFormat :: Applicative check => Format a -> Encoder check (EncoderImpl DecodeViaGet EncodeViaPut) a ByteString
haskFormat = Encoder . fmap haskImplFormat . checkFormat

haskImplFormat :: Format a -> EncoderImpl DecodeViaGet EncodeViaPut a ByteString
haskImplFormat = \case
  Format_Byte -> EncoderImpl
    { _encoderImpl_decode = decodeFromGet Binary.getWord8
    , _encoderImpl_encode = encodeFromPut Binary.putWord8
    }
  Format_Magic bs -> EncoderImpl
    { _encoderImpl_decode = decodeFromGet $ do
        --TODO: only consume until mismatch?
        bs' <- Binary.getByteString (fromIntegral $ ByteString.length bs)
        when (bs' /= ByteString.toStrict bs) $ fail "Magic number mismatch"
    , _encoderImpl_encode = encodeFromPut $ \() -> Binary.putByteString $ ByteString.toStrict bs
    }
  Format_Product fa fb ->
    let ea = haskImplFormat fa
        eb = haskImplFormat fb
    in EncoderImpl
       { _encoderImpl_encode = encodeFromPut $ \(a,b) -> do
           encodeToPut (_encoderImpl_encode ea) a
           encodeToPut (_encoderImpl_encode eb) b
       , _encoderImpl_decode = decodeFromGet $ do
           a <- decodeToGet (_encoderImpl_decode ea)
           b <- decodeToGet (_encoderImpl_decode eb)
           pure (a,b)
       }
  Format_Sum fa fb ->
    let ea = haskImplFormat fa
        eb = haskImplFormat fb
    in EncoderImpl
       { _encoderImpl_encode = encodeFromPut $ \case
           Left a -> Binary.putWord8 0 *> encodeToPut (_encoderImpl_encode ea) a
           Right b -> Binary.putWord8 1 *> encodeToPut (_encoderImpl_encode eb) b
       , _encoderImpl_decode = decodeFromGet $ do
           Binary.getWord8 >>= \case
             0 -> Left <$> decodeToGet (_encoderImpl_decode ea)
             1 -> Right <$> decodeToGet (_encoderImpl_decode eb)
             _ -> fail "Invalid tag for sum type"
       }
  Format_Replicate fn fa ->
    let en = haskImplFormat fn
        ea = haskImplFormat fa
    in EncoderImpl
       { _encoderImpl_encode = encodeFromPut $ \as -> do
           encodeToPut (_encoderImpl_encode en) (fromIntegral $ length as)
           for_ as $ encodeToPut (_encoderImpl_encode ea)
       , _encoderImpl_decode = decodeFromGet $ do
           n <- decodeToGet (_encoderImpl_decode en)
           replicateM (fromIntegral n) (decodeToGet (_encoderImpl_decode ea))
       }

checkFormat :: Applicative check => Format a -> check (Format a)
checkFormat f = case f of
  Format_Byte -> pure f
  Format_Magic _ -> pure f
  Format_Product _ _ -> pure f
  Format_Sum _ _ -> pure f
  Format_Replicate _ _ -> pure f

decodeFromGet :: Binary.Get a -> DecodeViaGet ByteString a
decodeFromGet = Categoryish_ImplicitSource $ \g -> Parse $ \bs -> case Binary.runGetOrFail g bs of
  Left (rest, cursor, err) -> Left (rest, cursor, T.pack err)
  Right (_, _, res) -> Right res

encodeFromPut :: (a -> Binary.Put) -> EncodeViaPut a ByteString
encodeFromPut = Categoryish_ImplicitTarget (\p -> Binary.runPut . getOp p) . Op

encodeToPut :: EncodeViaPut a ByteString -> (a -> Binary.Put)
encodeToPut (Categoryish_ImplicitTarget _ p) = getOp p

decodeToGet :: DecodeViaGet ByteString a -> Binary.Get a
decodeToGet (Categoryish_ImplicitSource _ g) = g

newtype Parse a b = Parse { runParse :: a -> Either (ByteString, Int64, Text) b }
instance Category Parse where
  id = Parse pure
  f . g = Parse $ runParse f <=< runParse g
instance Arrow Parse where
  arr f = Parse $ pure . f
  first (Parse f) = Parse $ \(a,b) -> fmap (,b) (f a)

decodeRust :: Format a -> Text
decodeRust f = snd $ runWriter $ flip runReaderT 0 $ do
  getByte
  eitherStruct
  line $ "fn decodeThing(buffer: &[u8]) -> Result<" <> decodedType f <> ", Text>{"
  indent $ do
    line "let cursor: usize = 0;"
    line "let sliceEnd: usize = 0;"
    go f
  line "}"

  where
    decodedType :: Format a -> Text
    decodedType = \case
      Format_Magic _ -> "()"
      Format_Byte -> "u8"
      Format_Product fa fb -> "(" <> decodedType fa <> "," <> decodedType fb <> ")"
      Format_Sum fa fb -> "Either<" <> decodedType fa <> "," <> decodedType fb <> ">"
      Format_Replicate _ fa -> "[" <> decodedType fa <> "]"

    eitherStruct = do
      line "enum Either<L,R> {"
      indent $ do
        line "Left(L),"
        line "Right(R),"
      line "}"

    getByte = do
      line "fn getByte(data: &[u8], cursor: &mut usize) -> u8 {"
      indent $ do
        line "let b = data[*cursor]; *cursor += 1;"
        line "b"

    go :: Format a -> ReaderT Word (Writer Text) ()
    go = \case
      Format_Magic bs -> scope $ do
        line $ "sliceEnd = cursor + " <> T.pack (show $ ByteString.length bs) <> ";"
        line $ "if(&buffer[cursor..sliceEnd] != " <> T.pack (show $ ByteString.unpack bs) <> ") {"
--        indent $ do
--          line "return Err(\"Magic number mismatch\")"
        line "}"
      Format_Byte -> tell "getByte()"
      Format_Product fa fb -> startScope "" $ do
        indented "let a = " *> go fa *> tell ";\n"
        indented "let b = " *> go fb *> tell ";\n"
        line "(a,b)"
      Format_Sum fa fb -> startScope "" $ do
        line "let tag = getByte();"
        startScope "if(tag == 0) " $ do
          indented "let res = " *> go fa *> tell ";\n"
          line "Left(res)"
        startScope "if(tag == 1) " $ do
          indented "let res = " *> go fb *> tell ";\n"
          line "Right(res)"
      Format_Replicate _ fa -> scope $ do
        line "let len = getByte();"
        startScope "for(uint i = 0; i < len; i++)" $
          go fa

decideSolidity :: Format a -> Text
decideSolidity f = snd $ runWriter $ flip runReaderT 0 $ do
  line "function decideThing(bytes memory buffer) {"
  indent $ do
    line "uint cursor = 0;"
    go f
    line "return true;"
  line "}"
  where
    failure :: Text
    failure = "return false;"
    go :: Format a -> ReaderT Word (Writer Text) ()
    go = \case
      Format_Magic bs -> sequence_ $ reverse $ ByteString.foldl (\xs x -> line (check x) : xs) [] bs
        where
          check b = "if(buffer[cursor++] != " <> T.pack (show b) <> ") { " <> failure <> " }"
      Format_Byte -> line "cursor++;"
      Format_Product fa fb -> go fa *> go fb
      Format_Sum fa fb -> do
        line "bytes1 tag = buffer[cursor++]"
        line "if(tag == 0) {"
        indent (go fa)
        line "} else if(tag == 1) {"
        indent (go fb)
        line "} else { "
        indent (line failure)
        line "}"
      Format_Replicate _ fa -> do
        line "byte len = buffer[cursor++];"
        line "for(uint i = 0; i < len; i++) {"
        indent $
          go fa
        line "}"

startScope :: (MonadReader Word m, MonadWriter Text m) => Text -> m a -> m a
startScope header body = do
  line $ header <> "{"
  res <- indent body
  line "}"
  pure res

scope :: (MonadReader Word m, MonadWriter Text m) => m a -> m a
scope body = do
  line "{"
  res <- local (+2) body
  line "}"
  pure res

indent :: MonadReader Word m => m a -> m a
indent = local (+2)

indented :: (MonadReader Word m, MonadWriter Text m) => Text -> m ()
indented t = do
  n <- ask
  tell $ T.replicate (fromIntegral n) " "
  tell t

line :: (MonadReader Word m, MonadWriter Text m) => Text -> m ()
line t = do
  indented t
  tell "\n"
