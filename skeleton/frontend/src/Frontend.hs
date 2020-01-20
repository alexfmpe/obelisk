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

import Control.Applicative (liftA2)
import Control.Lens (FunctorWithIndex(..), makePrisms, preview, set, (^?), _Left, _Right)
import Control.Monad (ap, replicateM, when, (<=<))
import Control.Monad.Fix
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Align
import Data.Functor
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Extend
import Data.Hourglass
import Data.Maybe (fromMaybe)
import Data.These
import Data.Tuple (swap)
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (wrap, workflow, workflowView)
import Reflex.Network

import Common.Route

--------------------------------------------------------------------------------
-- Wizard workflows
--------------------------------------------------------------------------------
newtype Wizard (t :: *) m a = Wizard { unWizard :: m (WizardInternal t m a) } deriving Functor
data WizardInternal t m a
  = WizardInternal_Terminal a
  | WizardInternal_Update (Event t a)
  | WizardInternal_Replace (Event t (Wizard t m a))
  deriving Functor
makePrisms ''WizardInternal

wizard :: (Reflex t, Functor m) => m (Event t a) -> Wizard t m a
wizard = Wizard . fmap WizardInternal_Update

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
-- Stack workflows
--------------------------------------------------------------------------------
newtype Stack (t :: *) m a = Stack { unStack :: m (StackInternal t m a) } deriving Functor
data StackInternal t m a
  = StackInternal_Now a
  | StackInternal_Later (Event t a)
  deriving Functor
makePrisms ''StackInternal

stack :: Functor m => m (Event t a) -> Stack t m a
stack = Stack . fmap StackInternal_Later

runStack :: PostBuild t m => Stack t m a -> m (Event t a)
runStack w = unStack w >>= \case
  StackInternal_Now a -> (a <$) <$> getPostBuild
  StackInternal_Later ev -> pure ev

instance (Functor m, Reflex t) => Apply (Stack t m) where
  (<.>) = undefined

instance (Applicative m, Reflex t) => Applicative (Stack t m) where
  pure = Stack . pure . StackInternal_Now
  (<*>) = (<.>)

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Bind (Stack t m) where
  join mm = stack $ do
    mEv <- runStack mm
    ((), ev) <- runWithReplace blank $ unStack <$> mEv
    let now = fmapMaybe (^? _StackInternal_Now) ev
    later <- switchHold never $ fmapMaybe (^? _StackInternal_Later) ev
    pure $ leftmost [now, later]

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Monad (Stack t m) where
  (>>=) = (>>-)

--------------------------------------------------------------------------------
-- Counter workflows
--------------------------------------------------------------------------------
newtype Counter (t :: *) m a = Counter { unCounter :: m (CounterInternal t m a) } deriving Functor
data CounterInternal t m a = CounterInternal
  { _counter_initialValue :: a
  , _counter_updates :: Event t a
  , _counter_replacements :: Event t (Counter t m a)
  } deriving Functor

counter :: (Monad m, PostBuild t m) => a -> m (Event t (Counter t m a)) -> Counter t m a
counter a m = Counter $ CounterInternal a never <$> m

runCounter :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m) => Counter t m a -> m (a, Event t a)
runCounter w = mdo
  (wint0, wintEv) <- runWithReplace (unCounter w) (fmap unCounter replacements)
  replacements <- switchHold (_counter_replacements wint0) (_counter_replacements <$> wintEv)
  updates <- switchHold (_counter_updates wint0) (_counter_updates <$> wintEv)
  pure (_counter_initialValue wint0, leftmost [_counter_initialValue <$> wintEv, updates `difference` replacements])

counterView :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Counter t m a -> m (Event t a)
counterView c = do
  postBuildEv <- getPostBuild
  (initialValue, replaceEv) <- runCounter c
  pure $ leftmost [initialValue <$ postBuildEv, replaceEv]

counterHold :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m) => Counter t m a -> m (Dynamic t a)
counterHold = uncurry holdDyn <=< runCounter

instance (Reflex t, Functor m, PostBuild t m) => Extend (Counter t m) where
  duplicated (Counter w) = Counter $ (fmap . fmap) pure w

instance (PostBuild t m) => Applicative (Counter t m) where
  pure a = Counter $ pure $ CounterInternal a never never
  (<*>) = undefined --ap --(<.>)

instance (Reflex t, Functor m) => Apply (Counter t m) where
  (<.>) = undefined

instance (Reflex t, Apply m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Bind (Counter t m) where
  join ww = Counter $ do
    wint <- unCounter ww
    (m0, mEv) <- runWithReplace (runCounter $ _counter_initialValue wint) (fmap runCounter $ _counter_updates wint)
    updates <- switchHold (snd m0) $ fmap snd mEv
    pure $ CounterInternal (fst m0) (leftmost [fmap fst mEv, updates]) (fmap join $ _counter_replacements wint)

instance (Reflex t, Apply m, Applicative m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Monad (Counter t m) where
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

-- for testing
counterOverlap :: (Monad m, PostBuild t m) => a -> m (Event t (Counter t m a)) -> Counter t m a
counterOverlap a m = Counter $ do
  x <- m
  pure $ CounterInternal a (a <$ x) x

replicator :: (DomBuilder t m, PostBuild t m) => a -> Int -> m (Event t a) -> Counter t m a
replicator a n w = counter a $ elAttr "div" ("style" =: "display:flex") $ do
  evs <- replicateM n w
  pure $ ffor (leftmost evs) $ \x -> replicator x (n + 1) w

digit :: (Show a, DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => (a -> a) -> Event t () -> a -> Counter t m a
digit succ' ev d = counter d $ do
  inc <- button $ tshow d
  innerStateWitness
  br
  pure $ digit succ' ev (succ' d) <$ leftmost [ev, inc]

year :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Counter t m Int
year = digit succ

month :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Month -> Counter t m Month
month = digit $ \m -> toEnum $ succ (fromEnum m) `mod` 12

day :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Month -> Int -> Counter t m Int
day ev y m = flip digit ev $ \d -> toEnum $ succ $ toEnum d `mod` daysInMonth y m

br :: DomBuilder t m => m ()
br = el "br" blank

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
workflowView :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Event t a)
workflowView = fmap snd . runWorkflow

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
          w

        example name w = do
          el "strong" $ text name
          br
          w
          br
          br

        justShow = display <=< holdDyn Nothing . fmap Just

        btn x = (x <$) <$> button x

        choice x = elAttr "div" ("style" =: "display:flex; margin-right: 15px") $ do
          a <- btn $ x <> ".A"
          b <- btn $ x <> ".B"
          br
          pure $ leftmost [a,b]

        choices mkWorkflow = do
          x0 <- mkWorkflow "_"
          x1 <- mkWorkflow x0
          x2 <- mkWorkflow x1
          x3 <- mkWorkflow x2
          pure x3

      section "Wizard" $ do
        example "Choices" $ do
          justShow <=< runWizard $ choices $ wizard . choice

      section "Stack" $
        example "Choices" $ do
          justShow <=< runStack $ choices $ stack . choice

      section "Counter" $ do
        example "Choices: replicator" $ do
          display <=< counterHold $ choices $ replicator "_" 1 . choice

        example "Calendar" $ mdo
          ymd <- counterHold $ do
            y <- year clk 2000
            m <- month clk January
            d <- day clk y m 27
            pure (y,m,d)
          dynText $ ffor ymd $ \(y,m,d) -> T.intercalate "-" $ [tshow y, tshow m, tshow d]
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
