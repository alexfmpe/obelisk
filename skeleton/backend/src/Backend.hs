{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Backend where

import Prelude hiding (length, id, snd, (.))

import Control.Category
import Control.Categorical.Bifunctor
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector
import Data.Universe
import Data.Word

import Common.Route
import Common.Serialization
import Obelisk.Backend
import Obelisk.Route

import Debug.Trace

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

suit :: Applicative check => EncoderK check Format Suit Word8
suit = enum "Suit"

rank :: Applicative check => EncoderK check Format Rank Word8
rank = enum "Rank"

card :: Applicative check => EncoderK check Format Card Word8
card = rank /\ suit

wtf :: Applicative check => EncoderK check Format Card Word8
wtf = id . rank . id /\ id . suit . id --card

ex1 :: Applicative check => EncoderK check Format Hand Word8
--ex1 = id . ((card /\ card /\ card) . id)
ex1 = (card /\ card /\ card)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> do
      threadDelay 1e4

      putStrLn mempty

      case checkEncoder ex1 of
        Left err -> print $ Text.unpack err
        Right enc -> do
          let
            fmt = extractEncoder enc
            impl = toEncoderBytes fmt
            e = encode @(StateT Word (Either Text)) impl $
              (Ace, Spades) :. (Queen, Hearts) :. (Two, Clubs)
            d = flip evalStateT 0 $ tryDecode impl $ e

            partial df = do
              putStrLn ""
--              putStrLn $ explain $ fst $ flip runState e $ toHaskBytes df fmt
              print $ dig @(Either Text) df e fmt

          putStrLn $ explain fmt
          print e
          print d

          partial $ Snd . Snd
          partial $ first id
          partial $ first Snd

  , _backend_routeEncoder = fullRouteEncoder
  }

dig :: MonadError Text m => Deformatting Word8 a b -> Vector Word8 -> Format a Word8 -> m b
dig d v f = do
  let
    (f', v') = flip runState v $ toHaskBytes d f
    impl = toEncoderBytes f'
  flip evalStateT 0 $ tryDecode impl $ trace (show v') v'
