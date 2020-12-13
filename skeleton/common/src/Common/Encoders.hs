{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Common.Encoders where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable
import Data.HexString
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

type SomeData = Either () () :. Word8 :. Either () Word8 :. [Word8]

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


    b = encodeHask f a
    c = decodeHask f b
  print a
  print b
  print $ fromBytes $ ByteString.toStrict b
  print c
  putStrLn ""
  putStrLn $ T.unpack $ decideSolidity f
  putStrLn $ T.unpack $ decodeRust f


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
  Format_Replicate :: Format Word8 -> Format a -> Format [a]

optional :: Format a -> Format (Either () a)
optional f = Format_Sum epsilon f

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
mkHaskBinaryEncoder = unsafeMkEncoder . (pure EncoderImpl <*> decodeHask <*> encodeHask)

encodeHask :: Format a -> (a -> ByteString)
encodeHask f = Binary.runPut . go f
  where
    go :: Format a -> (a -> Binary.Put)
    go = \case
      Format_Magic bs -> \() -> Binary.putByteString $ ByteString.toStrict bs
      Format_Byte -> Binary.put
      Format_Product fa fb -> \(a,b) -> go fa a *> go fb b
      Format_Sum fa fb -> \case
        Left a -> Binary.putWord8 0 *> go fa a
        Right b -> Binary.putWord8 1 *> go fb b
      Format_Replicate fn fa -> \as -> do
        go fn (fromIntegral $ length as)
        for_ as $ go fa

decodeHask :: Format a -> (ByteString -> a)
decodeHask = Binary.runGet . go
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
      Format_Replicate fn fa -> do
        n <- go fn
        replicateM (fromIntegral n) (go fa)

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
