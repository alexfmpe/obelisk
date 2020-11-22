{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad (when, (<=<))
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
  , _frontend_body = do
      let
        example name w = do
          el "strong" $ text name
          br
          void w
          br
          br

        justShow = display <=< holdDyn Nothing . fmap Just

        btn x = (x <$) <$> button x

        choice = choiceStyle $ const mempty
        choiceFade = choiceStyle $ \x -> if x == "_" then "opacity:0.5;" else mempty

        choiceStyle style x = elAttr "div" ("style" =: ("display:flex; margin-right: 15px;" <> style x)) $ do
          a <- btn $ x <> ".A"
          b <- btn $ x <> ".B"
          br
          pure $ leftmost [a,b]

        choiceFork x = el "div" $ do
          a <- btn $ x <> ".A"
          b <- btn $ x <> ".B"
          ab <- btn $ x <> ".*"
          z <- btn "Stop"
          br
          pure $ leftmost [a,b,ab,z]

        controls = do
          yes <- button "Replay"
          no <- button "Continue"
          stopEv <- button "Stop"
          br
          pure $ leftmost [Just True <$ yes, Just False <$ no, Nothing <$ stopEv]

        choices mkControls mkLayer = do
          start <- replay
          x0 <- mkLayer "0"
          x1 <- mkLayer x0
          mkControls >>= \case
            Just True -> start
            Just False -> pure ()
            Nothing -> stop
          x2 <- mkLayer x1
          x3 <- mkLayer x2
          pure x3

        machineInstant a ma = machine $ do
          pb <- getPostBuild
          ev <- ma
          pure $ leftmost [a <$ pb, ev]

      example "Wizard" $
        justShow <=< runWizard $ choices (machine controls) (machine . choice)

      example "Stack" $
        justShow <=< runStack $ choices
          (machine controls)
          (machine . choice)

      example "DFS" $
        justShow <=< runDFS $ choices
          (machine controls)
          (machine . choice)

      example "Wizard: postBuild" $
        justShow <=< runWizard $ pure "_"

      example "Stack: postBuild" $
        justShow <=< runStack $ choices
          (machine controls)
          (machineInstant "_" . choiceFade)

      example "Wizard of stacks" $ do
        justShow <=< runWizard $ do
          x <- machine $ runStack $ choices (pure $ Just False) $ machine . choice
          y <- machine $ runStack $ choices (pure $ Just False) $ machine . choice
          pure (x,y)

      example "Stack of workflows" $ mdo
        ymd <- runStack $ do
          y <- machine . workflowView $ year clk 2000
          m <- machine . workflowView $ month clk January
          d <- machine . workflowView $ day clk y m 27
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

      example "Forking the timelines" $ do
        let
          choices' mkControls mkLayer mkFork = do
            start <- replay
            x0 <- mkFork "0"
            x0' <- if ".*" `T.isSuffixOf` x0
                   then fork "0.A" "0.B"
                   else pure x0

            x1 <- mkLayer x0'
            mkControls >>= \case
              Just True -> start
              Just False -> pure ()
              Nothing -> stop
            x2 <- mkLayer x1
            x3 <- mkLayer x2
            pure x3

        let w = choices' (machine controls) (machine . choice) (machine . choiceFork)
            s = choices' (machine controls) (machine . choice) (machine . choiceFork)

        example "Wizard fork" $ justShow <=< runWizard $ w
        example "Stack fork" $ justShow <=< runStack $ s
  }
