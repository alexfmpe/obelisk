{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad (join, when, (<=<))
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Fix (MonadFix)
import Data.Functor (void)
import Data.Functor.Bind (Apply(..))
import Data.Hourglass (Month(..), daysInMonth)
import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
tshow :: Show a => a -> T.Text
tshow = T.pack . show

innerStateWitness :: DomBuilder t m => m ()
innerStateWitness = when False $ void $ inputElement $ def
  & inputElementConfig_initialValue .~ "some state"

digit :: (Show a, DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => (a -> a) -> Event t () -> a -> Workflow t m a
digit succ' ev d = Workflow $ do
  inc <- button $ tshow d
  innerStateWitness
  br
  pure (d, digit succ' ev (succ' d) <$ leftmost [ev, inc])

year :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Workflow t m Int
year = digit succ

month :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Month -> Workflow t m Month
month = digit $ \m -> toEnum $ succ (fromEnum m) `mod` 12

day :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Month -> Int -> Workflow t m Int
day ev y m = flip digit ev $ \d -> toEnum $ succ $ toEnum d `mod` daysInMonth y m

br :: DomBuilder t m => m ()
br = el "br" blank

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------
instance Monad m => Apply (RoutedT t r m) where
  (<.>) = (<*>)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Workflow examples"
  , _frontend_body = prerender_ blank $ do
      let
        example name w = do
          el "strong" $ text name
          br
          void w
          br
          br

        justShow = display <=< holdDyn Nothing . fmap Just

        btn x = mdo
          let txt = ffor cnt $ \c -> x <> " (" <> tshow c <> ")"
          clk <- switchHold never <=< dyn $ ffor txt $ \t -> (x <$) <$> button t
          cnt <- count clk
          pure clk

        choice = choiceStyle $ const mempty
        choiceFade = choiceStyle $ \x -> if x == "_" then "opacity:0.5;" else mempty

        buttons style = elAttr "div" ("style" =: ("display:flex; margin-right: 15px;" <> style))

        choiceStyle style x = buttons (style x) $ do
          a <- btn $ x <> ".A"
          b <- btn $ x <> ".B"
          br
          pure $ leftmost [a,b]

        choiceFork x = buttons "" $ do
          a <- btn $ x <> ".A"
          b <- btn $ x <> ".B"
          ab <- btn $ x <> ".*"
          z <- btn "Stop"
          br
          pure $ leftmost [a,b,ab,z]

        controls = buttons "" $ do
          yes <- button "Replay"
          no <- button "Continue"
          stopEv <- button "Stop"
          br
          pure $ leftmost [Just True <$ yes, Just False <$ no, Nothing <$ stopEv]

        choices mkControls mkLayer = evalContT $ do
          start <- label_
          x0 <- lift $ mkLayer "0"
          callCC $ \exit -> do
            x1 <- lift $ mkLayer x0
            lift mkControls >>= \case
              Just True -> start
              Just False -> pure ()
              Nothing -> lift stop --exit "X"
            x2 <- lift $ mkLayer x1
            x3 <- lift $ mkLayer x2
            pure x3

        stepInstant a ma = step $ do
          pb <- getPostBuild
          ev <- ma
          pure $ leftmost [a <$ pb, ev]

      example "Wizard" $
        justShow <=< wizard $ choices
          (step controls)
          (step . choice)

      example "Stack" $
        justShow <=< stack $ choices
          (step controls)
          (step . choice)
{-
      example "Breadth First" $
        justShow <=< breadthFirst $ choices
          (step controls)
          (step . choice)

      example "Depth First" $
        justShow <=< depthFirst $ choices
          (step controls)
          (step . choice)
-}
      example "Wizard: postBuild" $
        justShow <=< wizard $ pure "_"

      example "Stack: postBuild" $
        justShow <=< stack $ choices
          (step controls)
          (stepInstant "_" . choiceFade)

      example "Wizard of stacks" $ do
        justShow <=< wizard $ do
          x <- step $ stack $ choices (pure $ Just False) $ step . choice
          y <- step $ stack $ choices (pure $ Just False) $ step . choice
          pure (x,y)

      example "Stack of workflows" $ mdo
        ymd <- stack $ do
          y <- step . workflowView $ year clk 2000
          m <- step . workflowView $ month clk January
          d <- step . workflowView $ day clk y m 27
          pure (y,m,d)
        widgetHold blank $ ffor ymd $ \(y,m,d) -> text $ T.intercalate "-" [tshow y, tshow m, tshow d]
        br
        display =<< count @_ @_ @Int ymd
        clk <- if True
          then pure never
          else do
          text " replacements"
          br
          button "trigger all simultaneously"
        pure ()
{-
      example "Round robin wizards" $ do
        justShow <=< wizard . uncurry roundRobin . join (,) $ choices
          (step controls)
          (step . choice)

      let ad = fmap void (button "Buy now!") <* br

      example "Before wizard" $ do
        justShow <=< wizard . before ad $ choices
          (step controls)
          (step . choice)

      example "After wizard" $ do
        justShow <=< wizard . after ad $ choices
          (step controls)
          (step . choice)
-}
      example "Forking the timelines" $ do
        let
          choices' mkControls mkLayer mkFork = evalContT $ do
            start <- label_
            x0 <- mkFork "0"
            x0' <- case x0 of
              "Stop" -> lift stop
--              x | ".*" `T.isSuffixOf` x -> fork ["0.A", "0.B"]
              x -> pure x
            x1 <- mkLayer x0
            mkControls >>= \case
              Just True -> start
              Just False -> pure ()
              Nothing -> lift stop
            x2 <- mkLayer x1
            x3 <- mkLayer x2
            pure x3

        let c = choices' (lift $ step controls) (lift . step . choice) (lift . step . choiceFork)

        example "Wizard fork" $ justShow <=< wizard $ c
        example "Stack fork" $ justShow <=< stack $ c
--        example "Breadth First fork" $ justShow <=< breadthFirst $ c
--        example "Depth First fork" $ justShow <=< depthFirst $ c
  }
