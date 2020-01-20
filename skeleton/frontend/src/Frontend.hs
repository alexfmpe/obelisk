{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Lens (FunctorWithIndex(..), makePrisms, preview)
import Control.Monad (ap, replicateM, when, (<=<), (>=>))
import Control.Monad.Fix
import Data.Align
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Hourglass
import Data.Maybe (fromMaybe)
import Data.These
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (wrap, workflow, workflowView)

import Common.Route

--------------------------------------------------------------------------------
-- Workflow
--------------------------------------------------------------------------------
-- | Runs a 'Workflow' and returns the initial value together with an 'Event' of the values produced by the whenever one 'Workflow' is replaced by another.
runWorkflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (a, Event t a)
runWorkflow w0 = mdo
  ((a, e0), eResult) <- runWithReplace (unWorkflow w0) (fmap unWorkflow eReplace)
  eReplace <- switchHold e0 $ fmap snd eResult
  return (a, fmap fst eResult)

-- | Similar to 'runWorkflow' but combines the result into a 'Dynamic'.
workflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow = uncurry holdDyn <=< runWorkflow

-- | Similar to 'workflow', but only returns the 'Event'.
workflowView :: (Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w = do
  postBuildEv <- getPostBuild
  (initialValue, replaceEv) <- runWorkflow w
  pure $ leftmost [initialValue <$ postBuildEv, replaceEv]

--------------------------------------------------------------------------------
-- Wizard
--------------------------------------------------------------------------------
newtype Wizard t m a = Wizard { unWizard :: m (WizardInternal t m a) } deriving Functor
data WizardInternal t m a
  = WizardInternal_Terminal a
  | WizardInternal_Update (Event t a)
  | WizardInternal_Replace (Event t (Wizard t m a))
  deriving Functor
makePrisms ''WizardInternal

step :: Functor m => m (Event t a) -> Wizard t m a
step = Wizard . fmap WizardInternal_Update

runWizard :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Wizard t m a -> m (Event t a)
runWizard w = mdo
  let getReplace = fromMaybe never . preview _WizardInternal_Replace
      getUpdate = fromMaybe never . preview _WizardInternal_Update
  (wint0, wintEv) <- runWithReplace (unWizard w) $ leftmost [unWizard <$> replace, pure . WizardInternal_Terminal <$> updates]
  replace <- switchHold (getReplace wint0) (getReplace <$> wintEv)
  updates <- switchHold (getUpdate wint0) (getUpdate <$> wintEv)
  pb <- getPostBuild
  let terminal0 = maybe never (<$ pb) $ preview _WizardInternal_Terminal wint0
      terminal = fmapMaybe (preview _WizardInternal_Terminal) wintEv
  pure $ leftmost [terminal0, terminal]

instance (Functor m, Reflex t) => Apply (Wizard t m) where
  (<.>) = undefined

instance (Applicative m, Reflex t) => Applicative (Wizard t m) where
  pure = Wizard . pure . WizardInternal_Terminal
  (<*>) = (<.>)

instance (Monad m, Reflex t) => Bind (Wizard t m) where
  join ww = Wizard $ unWizard ww >>= \case
    WizardInternal_Terminal (Wizard w) -> w
    WizardInternal_Update ev -> pure $ WizardInternal_Replace ev
    WizardInternal_Replace ev -> pure $ WizardInternal_Replace $ ffor ev join

instance (Monad m, Reflex t) => Monad (Wizard t m) where
  (>>=) = (>>-)

--------------------------------------------------------------------------------
-- Stack
--------------------------------------------------------------------------------
newtype Stack t m a = Stack { unStack :: m (Maybe a, Event t a) } deriving Functor

frame :: m (Maybe a, Event t a) -> Stack t m a
frame = Stack

stackView :: PostBuild t m => Stack t m a -> m (Event t a)
stackView = unStack >=> \case
  (Nothing, ev) -> pure ev
  (Just a, ev) -> do
    pb <- getPostBuild
    pure $ leftmost [a <$ pb, ev]

instance (Functor m, Reflex t) => Apply (Stack t m) where
  (<.>) = undefined

instance (Applicative m, Reflex t) => Applicative (Stack t m) where
  pure = Stack . pure . (, never) . Just
  (<*>) = (<.>)

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Bind (Stack t m) where
  join ss = frame $ do
    (ms0, sEv) <- unStack ss
    case ms0 of
      Nothing -> do
        ((), ev) <- runWithReplace blank $ unStack <$> sEv
        e <- switchHold never $ fmap snd ev
        pure (Nothing, leftmost [fmapMaybe fst ev, e])
      Just s0 -> do
        ((a0,ev0), ev) <- runWithReplace (unStack s0) $ unStack <$> sEv
        e <- switchHold never $ fmap snd ev
        pure (a0, leftmost [ev0, fmapMaybe fst ev, e])

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Monad (Stack t m) where
  (>>=) = (>>-)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
tshow :: Show a => a -> T.Text
tshow = T.pack . show

innerStateWitness :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
innerStateWitness = when False $ do
  c <- count =<< button "increment inner state"
  dyn_ $ ffor c $ \(j :: Int) -> text $ tshow j

digit :: (Show a, DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => (a -> a) -> Event t () -> a -> Workflow t m a
digit succ' ev d = Workflow $ do
  inc <- button $ tshow d
  innerStateWitness
  br
  pure $ (d, digit succ' ev (succ' d) <$ leftmost [ev, inc])

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

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Workflow examples"
  , _frontend_body = do
      let
        section name w = do
          el "h3" $ text $ name <> " workflows"
          void w

        example name w = do
          el "strong" $ text name
          br
          void w
          br
          br

        justHold x = holdDyn x . fmap Just
        justShow = display <=< justHold Nothing

        btn x = (x <$) <$> button x

        choice = choiceStyle $ const mempty
        choiceFade = choiceStyle $ \x -> if x == "_" then "opacity:0.5;" else mempty

        choiceStyle style x = elAttr "div" ("style" =: ("display:flex; margin-right: 15px;" <> style x)) $ do
          a <- btn $ x <> ".A"
          b <- btn $ x <> ".B"
          br
          pure $ leftmost [a,b]

        choices mkWorkflow = do
          x0 <- mkWorkflow "0"
          x1 <- mkWorkflow x0
          x2 <- mkWorkflow x1
          x3 <- mkWorkflow x2
          pure x3

        frameFromWorkflow = frame . fmap (first Just) . runWorkflow

      section "Wizard" $ do
        example "Choices" $ do
          justShow <=< runWizard $ choices $ step . choice

      section "Stack" $ do
        example "Choices" $
          justShow <=< stackView $ choices $ frame . fmap (Nothing,) . choice
        example "Choices with initial value" $
          justShow <=< stackView $ choices $ frame . fmap (Just "_",) . choiceFade

        example "Calendar" $ mdo
          ymd <- stackView $ do
            y <- frameFromWorkflow $ year clk 2000
            m <- frameFromWorkflow $ month clk January
            d <- frameFromWorkflow $ day clk y m 27
            pure (y,m,d)
          justShow $ ffor ymd $ \(y,m,d) -> T.intercalate "-" $ [tshow y, tshow m, tshow d]
          br
          display =<< count @_ @_ @Int ymd
          clk <- if True
            then pure never
            else do
            text " replacements"
            br
            button "trigger all simultaneously"
          pure ()
  }
