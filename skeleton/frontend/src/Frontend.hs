{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad (when, (<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Witherable (catMaybes)
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
-- Workflow
--------------------------------------------------------------------------------
-- | Similar to 'runWorkflow' but combines the result into a 'Dynamic'.
workflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow = uncurry holdDyn <=< runWorkflow

-- | Similar to 'runWorkflow', but also puts the initial value in the 'Event'.
workflowView :: (Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w = do
  postBuildEv <- getPostBuild
  (initialValue, replaceEv) <- runWorkflow w
  pure $ leftmost [initialValue <$ postBuildEv, replaceEv]

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

        controls = do
          yes <- button "Replay"
          no <- button "Stop"
          br
          pure $ leftmost [True <$ yes, False <$ no]

        choices mkControls mkLayer = do
          start <- replay
          x0 <- mkLayer "0"
          x1 <- mkLayer x0
          x2 <- mkLayer x1
          x3 <- mkLayer x2
          again <- mkControls
          when again
            start
          pure x3

        frameIncremental :: (Adjustable t m, MonadHold t m, PostBuild t m) => m (Event t a) -> Stack t m (Maybe a)
        frameIncremental = frame . fmap (\ev -> (Nothing, fmap Just ev))

      example "Wizard" $
        justShow <=< runWizard $ choices (step controls) (step . choice)

      example "Stack: fixed layers" $
        display <=< stackHold $ choices
          (frame . fmap (False,) $ controls)
          (frame . fmap ("_",) . choiceFade)

      example "Stack: incremental layers via MaybeT" $
        display <=< stackHold $ runMaybeT $ choices
          (MaybeT . frameIncremental $ controls)
          (MaybeT . frameIncremental . choice)

      example "Wizard of incremental stacks" $ do
        justShow <=< runWizard $ do
          x <- step $ fmap catMaybes $ stackView $ runMaybeT $ choices (pure False) $ MaybeT . frameIncremental . choice
          y <- step $ fmap catMaybes $ stackView $ runMaybeT $ choices (pure False) $ MaybeT . frameIncremental . choice
          pure (x,y)

      example "Stack of workflows" $ mdo
         ymd <- stackHold $ do
           y <- frame . runWorkflow $ year clk 2000
           m <- frame . runWorkflow $ month clk January
           d <- frame . runWorkflow $ day clk y m 27
           pure (y,m,d)
         dynText $ ffor ymd $ \(y,m,d) -> T.intercalate "-" [tshow y, tshow m, tshow d]
         br
         display =<< count @_ @_ @Int (updated ymd)
         clk <- if True
           then pure never
           else do
           text " replacements"
           br
           button "trigger all simultaneously"
         pure ()
  }
