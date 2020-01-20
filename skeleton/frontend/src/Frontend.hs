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

import Control.Lens (FunctorWithIndex(..), makePrisms, preview, set, (^?), _Left, _Right)
import Control.Monad (ap, replicateM, when, (<=<))
import Control.Monad.Fix
import Data.Align
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
-- Wizard workflows
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
-- Stack workflows
--------------------------------------------------------------------------------
newtype Stack t m a = Stack { unStack :: m (StackInternal t m a) } deriving Functor
data StackInternal t m a
  = StackInternal_Now a
  | StackInternal_Later (Event t a)
  deriving Functor
makePrisms ''StackInternal

frame :: Functor m => m (Event t a) -> Stack t m a
frame = Stack . fmap StackInternal_Later

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
  join mm = frame $ do
    mEv <- runStack mm
    ((), ev) <- runWithReplace blank $ unStack <$> mEv
    let now = fmapMaybe (^? _StackInternal_Now) ev
    later <- switchHold never $ fmapMaybe (^? _StackInternal_Later) ev
    pure $ leftmost [now, later]

instance (Adjustable t m, MonadHold t m, PostBuild t m) => Monad (Stack t m) where
  (>>=) = (>>-)

--------------------------------------------------------------------------------
-- Hierarchy workflows
--------------------------------------------------------------------------------
newtype Hierarchy t m a = Hierarchy { unHierarchy :: m (HierarchyInternal t m a) } deriving Functor
data HierarchyInternal t m a = HierarchyInternal
  { _hierarchy_initialValue :: a
  , _hierarchy_updates :: Event t a
  , _hierarchy_replacements :: Event t (Hierarchy t m a)
  } deriving Functor

hierarchy :: (Monad m, PostBuild t m) => a -> m (Event t (Hierarchy t m a)) -> Hierarchy t m a
hierarchy a m = Hierarchy $ HierarchyInternal a never <$> m

runHierarchy :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m) => Hierarchy t m a -> m (a, Event t a)
runHierarchy w = mdo
  (wint0, wintEv) <- runWithReplace (unHierarchy w) (fmap unHierarchy replacements)
  replacements <- switchHold (_hierarchy_replacements wint0) (_hierarchy_replacements <$> wintEv)
  updates <- switchHold (_hierarchy_updates wint0) (_hierarchy_updates <$> wintEv)
  pure (_hierarchy_initialValue wint0, leftmost [_hierarchy_initialValue <$> wintEv, updates `difference` replacements])

hierarchyView :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Hierarchy t m a -> m (Event t a)
hierarchyView c = do
  postBuildEv <- getPostBuild
  (initialValue, replaceEv) <- runHierarchy c
  pure $ leftmost [initialValue <$ postBuildEv, replaceEv]

hierarchyHold :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m) => Hierarchy t m a -> m (Dynamic t a)
hierarchyHold = uncurry holdDyn <=< runHierarchy

instance (Reflex t, Functor m, PostBuild t m) => Extend (Hierarchy t m) where
  duplicated (Hierarchy w) = Hierarchy $ (fmap . fmap) pure w

instance (PostBuild t m) => Applicative (Hierarchy t m) where
  pure a = Hierarchy $ pure $ HierarchyInternal a never never
  (<*>) = undefined --ap --(<.>)

instance (Reflex t, Functor m) => Apply (Hierarchy t m) where
  (<.>) = undefined

instance (Reflex t, Apply m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Bind (Hierarchy t m) where
  join ww = Hierarchy $ do
    wint <- unHierarchy ww
    (m0, mEv) <- runWithReplace (runHierarchy $ _hierarchy_initialValue wint) (fmap runHierarchy $ _hierarchy_updates wint)
    updates <- switchHold (snd m0) $ fmap snd mEv
    pure $ HierarchyInternal (fst m0) (leftmost [fmap fst mEv, updates]) (fmap join $ _hierarchy_replacements wint)

instance (Reflex t, Apply m, Applicative m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Monad (Hierarchy t m) where
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
hierarchyOverlap :: (Monad m, PostBuild t m) => a -> m (Event t (Hierarchy t m a)) -> Hierarchy t m a
hierarchyOverlap a m = Hierarchy $ do
  x <- m
  pure $ HierarchyInternal a (a <$ x) x

replicator :: (DomBuilder t m, PostBuild t m) => a -> Int -> m (Event t a) -> Hierarchy t m a
replicator a n w = hierarchy a $ elAttr "div" ("style" =: "display:flex") $ do
  evs <- replicateM n w
  pure $ ffor (leftmost evs) $ \x -> replicator x (n + 1) w

digit :: (Show a, DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => (a -> a) -> Event t () -> a -> Hierarchy t m a
digit succ' ev d = hierarchy d $ do
  inc <- button $ tshow d
  innerStateWitness
  br
  pure $ digit succ' ev (succ' d) <$ leftmost [ev, inc]

year :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Hierarchy t m Int
year = digit succ

month :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Month -> Hierarchy t m Month
month = digit $ \m -> toEnum $ succ (fromEnum m) `mod` 12

day :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Month -> Int -> Hierarchy t m Int
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
          justShow <=< runWizard $ choices $ step . choice

      section "Stack" $
        example "Choices" $ do
          justShow <=< runStack $ choices $ frame . choice

      section "Hierarchy" $ do
        example "Choices: replicator" $ do
          display <=< hierarchyHold $ choices $ replicator "_" 1 . choice

        example "Calendar" $ mdo
          ymd <- hierarchyHold $ do
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
