{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Backend where

import Prelude hiding (length, id, (.))

import Control.Concurrent
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Universe
import Data.Word

import Common.Route
import Common.Serialization
import Obelisk.Backend
import Obelisk.Route

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

ex1 :: Applicative check => EncoderK check Format Hand Word8
ex1 = card /\ card /\ card

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> do
      threadDelay 1e4

      putStrLn mempty

      case checkEncoder ex1 of
        Left err -> print $ Text.unpack err
        Right fmt -> do
          let
            enc = toEncoderBytes fmt
            e = encode @(StateT Word (Either Text)) enc $
              ((Ace, Spades), ((Queen, Hearts), (Two, Clubs)))
              --((Ace, Spades) /\ (Queen, Hearts) /\ (Two, Clubs))
            d = flip evalStateT 0 $ tryDecode enc $ e

          putStrLn $ explain fmt
          print e
          print d
  , _backend_routeEncoder = fullRouteEncoder
  }


{-
ex1 :: Format Card Word8
ex1 = card (Ace, Spades)
   /\ card (Queen, Hearts)
   /\  (  card (Two, Diamonds) /\ card (Three, Diamonds)
       \/ card (Two, Clubs)    /\ card (Three, Clubs)
       )
-}
