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
import Control.Monad (ap, (<=<))
import Control.Monad.Fix
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Align
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Extend
import Data.Maybe (fromMaybe)
import Data.These
import Data.Tuple (swap)
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (wrap)
import Reflex.Network

import Common.Route

--------------------------------------------------------------------------------
-- Workflow monad
--------------------------------------------------------------------------------
newtype W (t :: *) m a = W { unW :: m (WInternal t m a) } deriving Functor
data WInternal t m a
  = WInternal_Initial a
  | WInternal_Update (Event t a)
  | WInternal_Replace (Event t (W t m a))
  deriving Functor
makePrisms ''WInternal

prompt :: (Reflex t, Functor m) => m (Event t a) -> W t m a
prompt = W . fmap WInternal_Update

runW :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => W t m a -> m (Event t a)
runW w = mdo
  let getReplace = fromMaybe never . preview _WInternal_Replace
      getUpdate = fromMaybe never . preview _WInternal_Update
  (wint0, wintEv) <- runWithReplace (unW w) $ leftmost [unW <$> replace, pure . WInternal_Initial <$> updates]
  replace <- switchHold (getReplace wint0) (getReplace <$> wintEv)
  updates <- switchHold (getUpdate wint0) (getUpdate <$> wintEv)
  pb <- getPostBuild
  let initial0 = maybe never (<$ pb) $ preview _WInternal_Initial wint0
      initial = fmapMaybe (preview _WInternal_Initial) wintEv
  pure $ leftmost [initial0, initial]

instance (Functor m, Reflex t) => Apply (W t m) where
  (<.>) = undefined

instance (Applicative m, Reflex t) => Applicative (W t m) where
  pure = W . pure . WInternal_Initial
  (<*>) = (<.>)

instance (Monad m, Reflex t) => Bind (W t m) where
  join ww = W $ unW ww >>= \case
    WInternal_Initial (W w) -> w
    WInternal_Update ev -> pure $ WInternal_Replace ev
    WInternal_Replace ev -> pure $ WInternal_Replace $ ffor ev join

instance (Monad m, Reflex t) => Monad (W t m) where
  (>>=) = (>>-)

newtype W'' (t :: *) m a = W'' { unW'' :: m (WInternal t m a) } deriving Functor

prompt'' :: (Reflex t, Functor m) => m (Event t a) -> W'' t m a
prompt'' = W'' . fmap WInternal_Update
{-
runW'' :: forall t m a. (Adjustable t m, MonadHold t m, PostBuild t m) => W'' t m a -> m (a, Event t a)
runW'' (W'' w0) = runF w0
  (\l -> pure (l, never))
  (\(Compose r) -> do
      ev <- r
      d <- networkHold (pure never) ev
      switchHold never $ updated d
  )
-}



newtype W' (t :: *) m a = W' { unW' :: m (WInternal' t m a) } deriving Functor
data WInternal' t m a = WInternal'
  { _w_initialValue :: a
  , _w_updates :: Event t a
  , _w_replacements :: Event t (W' t m a)
  } deriving Functor

runW' :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m) => W' t m a -> m (a, Event t a)
runW' w = mdo
  (wint0, wintEv) <- runWithReplace (unW' w) (fmap unW' replacements)
  replacements <- switchHold (_w_replacements wint0) (_w_replacements <$> wintEv)
  updates <- switchHold (_w_updates wint0) (_w_updates <$> wintEv)
  pure (_w_initialValue wint0, leftmost [_w_initialValue <$> wintEv, updates `difference` replacements])

instance (Reflex t, Functor m, PostBuild t m) => Extend (W' t m) where
  duplicated (W' w) = W' $ (fmap . fmap) pure w

instance (PostBuild t m) => Applicative (W' t m) where
  pure a = W' $ pure $ WInternal' a never never
  (<*>) = undefined --ap --(<.>)

instance (Reflex t, Functor m) => Apply (W' t m) where
  (<.>) = undefined

instance (Reflex t, Apply m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Bind (W' t m) where
  join ww = W' $ do
    wint <- unW' ww
    (m0, mEv) <- runWithReplace (runW' $ _w_initialValue wint) (fmap runW' $ _w_updates wint)
    updates <- switchHold (snd m0) $ fmap snd mEv
    pure $ WInternal' (fst m0) (leftmost [fmap fst mEv, updates]) (fmap join $ _w_replacements wint)

instance (Reflex t, Apply m, Applicative m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Monad (W' t m) where
  (>>=) = (>>-)




frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      let
        justShow :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Event t Int -> m ()
        justShow = display <=< holdDyn Nothing . fmap Just

      clk <- button "replace all"

      br
      br
      text "Workflows - widget sequence semantics"
      br

      justShow <=< runW $ do
        x <- prompt $ do
          text $ tshow 0
          innerStateWitness
          ev <- button "Next"
          br
          pure $ 1 <$ ev
        y <- prompt $ do
          text $ tshow x
          innerStateWitness
          ev <- button "Next"
          br
          pure $ 2 <$ ev
        prompt $ do
          text $ tshow y
          innerStateWitness
          ev <- button "Next"
          br
          pure $ 3 <$ ev
--        prompt $ do
--          pure never
--        pure z

      br

      br
      br
      text "Workflows - widget menu semantics"

--      justShow <=< runW'' $ do
--        counterW'' clk 5 0
--        b <- counterW'' clk (a + 1) 0
--        counterW'' clk (b + 1) 0

      br
      br
      br
      br
      text "Workflows - widget hierarchy semantics"
      res <- runW' $ do
        pure ()
        a <- counterW' clk 5 0
        pure ()
        b <- counterW' clk (a + 1) 0
        counterW' clk (b + 1) 0
      br
      display =<< uncurry holdDyn res
      text " <- latest payload"
      br
      display =<< count @_ @_ @Int (snd res)
      text " <- payload updates"

      br
      br
      br
      br
      text "Workflows - PR #300 (broken - instances do not preserve inner state) "
      br
      let
        w2 = counterWorkflow clk 2 0
        w3 = counterWorkflow clk 3 0

      workflow w2 >>= display
      br
      workflow w3 >>= display
      br
      br


      renderW "<>" $ fmap show w2 <> pure " " <> fmap show w3
      renderW "<.>" $ (,) <$> w2 <*> w3

      renderW "<!>" $ w2 <!> w3
      renderW "join" $ join $ counterWorkflow2 clk 5 0

      renderW "<.> (reversed)" $ independentWorkflows (\(ma, mb) -> do
                                                          b <- mb
                                                          a <- ma
                                                          pure (a, b))
                                                      const
                                                      w2
                                                      w3
  }

tshow :: Show a => a -> T.Text
tshow = T.pack . show

counterWorkflow :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> Workflow t m Int
counterWorkflow ev n i = Workflow $ do
  inc <- button $ T.pack $ show i <> "/" <> show n
  innerStateWitness
  br
  pure (i, counterWorkflow ev n ((i + 1) `mod` n) <$ leftmost [ev, inc])

counterWorkflow2 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> Workflow t m (Workflow t m Int)
counterWorkflow2 ev n i = Workflow $ do
  next <- button "next"
  innerStateWitness
  pure (counterWorkflow ev (i + 1) 0, counterWorkflow2 ev n ((i + 1) `mod` n) <$ leftmost [ev, next])

counterW :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> W t m Int
counterW ev n i = W $ fmap WInternal_Replace $ do
  inc <- button $ T.pack $ show i <> "/" <> show n
  innerStateWitness
  pure $ (counterW ev n ((i + 1) `mod` n)) <$ leftmost [ev, inc]

counterW'' :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> W'' t m Int
counterW'' ev n i = W'' $ fmap WInternal_Replace $ do
  inc <- button $ T.pack $ show i <> "/" <> show n
  innerStateWitness
  br
  pure $ (counterW ev n ((i + 1) `mod` n)) <$ leftmost [ev, inc]

innerStateWitness :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
innerStateWitness = do
  c <- count =<< button "increment inner state"
  dyn_ $ ffor c $ \(j :: Int) -> text $ tshow j

-- for testing
mkWorkflowOverlap :: (Monad m, PostBuild t m) => a -> m (Event t (W' t m a)) -> W' t m a
mkWorkflowOverlap a m = W' $ do
  x <- m
  pure $ WInternal' a (a <$ x) x

mkWorkflow :: (Monad m, PostBuild t m) => a -> m (Event t (W' t m a)) -> W' t m a
mkWorkflow a m = W' $ WInternal' a never <$> m


counterW' :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Int -> W' t m Int
counterW' ev n i = mkWorkflow i $ do
  br
  inc <- button $ T.pack $ show i <> "/" <> show n
  innerStateWitness
  pure $ counterW' ev n ((i + 1) `mod` n) <$ leftmost [ev, inc]

br :: DomBuilder t m => m ()
br = el "br" blank

renderW :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Show a) => T.Text -> Workflow t m a -> m ()
renderW lbl w = do
  text lbl
  br
  ipayload <- if True
              then workflow' $ imap (,) w
              else uncurry holdDyn <=< workflowView' $ imap (,) w
  dynText $ ffor ipayload $ \(_, p) -> tshow p <> " <- latest payload"
  br
  dynText $ ffor ipayload $ \(k, _) -> tshow k <> " <- payload updates"
  br
  br

-- | Runs a 'Workflow' and returns the initial value together with an 'Event' of the values produced by the whenever one 'Workflow' is replaced by another.
runWorkflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (a, Event t a)
runWorkflow w0 = mdo
  ((a, e0), eResult) <- runWithReplace (unWorkflow w0) (fmap unWorkflow eReplace)
  eReplace <- switchHold e0 $ fmap snd eResult
  return (a, fmap fst eResult)

-- | Similar to 'runWorkflow' but combines the result into a 'Dynamic'.
workflow' :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow' = uncurry holdDyn <=< runWorkflow

-- | Similar to 'workflow', but only returns the 'Event'.
workflowView :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Event t a)
workflowView = fmap snd . runWorkflow

instance Monad m => Apply (RoutedT t r m) where
  (<.>) = (<*>)

--------------------------------------------------------------------------------
-- Upstream
--------------------------------------------------------------------------------

zipWidgets :: Apply f => (f a, f b) -> f (a,b)
zipWidgets (a,b) = liftF2 (,) a b

--------------------------------------------------------------------------------
-- Upstreamed
--------------------------------------------------------------------------------

workflowView' :: forall t m a. (NotReady t m, Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (a, Event t a)
workflowView' w0 = mdo
  ((a, e0), eResult) <- runWithReplace (unWorkflow w0) (fmap unWorkflow eReplace)
  eReplace <- switchHold e0 $ fmap snd eResult
  return (a, fmap fst eResult)

  {-
  (result, eResult) <- runWithReplace (unWorkflow w0) (fmap pure eResult)
  return (fst result, fmap fst eResult)
  -}
--------------------------------------------------------------------------------
-- Transforming workflows
--------------------------------------------------------------------------------
{-# DEPRECATED mapWorkflow "Use 'fmap' instead" #-}
-- | Map a function over a 'Workflow', possibly changing the return type.
mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow = fmap

--------------------------------------------------------------------------------
-- Combining occurrences
--------------------------------------------------------------------------------
withLastOccurrences :: (a -> b -> c) -> (a, b) -> These () () -> c
withLastOccurrences f (a,b) _ = f a b

theseOccurrence :: (a,b) -> These () () -> These a b
theseOccurrence (a,b) = set here a . set there b

leftmostOccurrence :: (a,a) -> These () () -> a
leftmostOccurrence (a,b) = \case
  This () -> a
  That () -> b
  These () () -> a

--------------------------------------------------------------------------------
-- Combining widgets
--------------------------------------------------------------------------------
forwardRender :: Apply f => (f a, f b) -> f (a,b)
forwardRender (a,b) = liftF2 (,) a b

backwardRender :: Apply f => (f a, f b) -> f (a,b)
backwardRender = fmap swap . forwardRender . swap

intercalateWidgets :: Apply f => f c -> (f a, f b) -> f (a,b)
intercalateWidgets c (a,b) = (,) <$> a <. c <.> b

--------------------------------------------------------------------------------
-- Combining workflows
--------------------------------------------------------------------------------
#if MIN_VERSION_these(0, 8, 0)
instance (Apply m, Reflex t) => Semialign (Workflow t m) where
  align = independentWorkflows forwardRender theseOccurrence
instance (Apply m, Reflex t) => AlignWithIndex Int (Workflow t m)
#endif

-- | Combine two workflows via `combineWorkflows`. Triggers of the first workflow reset the second one.
hierarchicalWorkflows
  :: (Functor m, Reflex t)
  => (forall x y. (m x, m y) -> m (x,y))
  -- ^ Compute resulting widget
  -> ((a,b) -> These () () -> c)
  -- ^ Compute resulting payload based on current payloads and ocurrences
  -> Workflow t m a
  -> Workflow t m b
  -> Workflow t m c
hierarchicalWorkflows = combineWorkflows $ \(_, wb0) (wa, _) -> \case
  This wa' -> (wa', wb0)
  That wb' -> (wa, wb')
  These wa' _ -> (wa', wb0)

zipWorkflows :: (Apply m, Reflex t) => Workflow t m a -> Workflow t m b -> Workflow t m (a,b)
zipWorkflows = zipWorkflowsWith (,)

{-
-- | Runs a 'Workflow' and returns the 'Dynamic' result of the 'Workflow' (i.e., a 'Dynamic' of the value produced by the current 'Workflow' node, and whose update 'Event' fires whenever one 'Workflow' is replaced by another).
workflow'' :: forall t m a. (Reflex t, Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow'' w0 = do
  rec eResult <- networkHold (unWorkflow w0) $ fmap unWorkflow $ switch $ snd <$> current eResult
  return $ fmap fst eResult
-}
{-
zipWorkflows' :: (Apply m, Reflex t) => Workflow t m a -> Workflow t m b -> Workflow t m (a,b)
zipWorkflows' wa wb = do
  (a0, waEv) <- unWorkflow wa
  (b0, wbEv) <- unWorkflow wb
-}


-- | Create a workflow that's replaced when either input workflow is replaced.
-- The value of the output workflow is obtained by applying the provided function to the values of the input workflows
zipWorkflowsWith :: (Apply m, Reflex t) => (a -> b -> c) -> Workflow t m a -> Workflow t m b -> Workflow t m c
zipWorkflowsWith f = independentWorkflows forwardRender (withLastOccurrences f)

-- | Combine two workflows via `combineWorkflows`. Triggers of one workflow do not affect the other one.
independentWorkflows
  :: (Functor m, Reflex t)
  => (forall x y. (m x, m y) -> m (x,y))
  -- ^ Compute resulting widget
  -> ((a,b) -> These () () -> c)
  -- ^ Compute resulting payload based on current payloads and ocurrences
  -> Workflow t m a
  -> Workflow t m b
  -> Workflow t m c
independentWorkflows = combineWorkflows $ \(_, _) (wa, wb) -> fromThese wa wb

-- | Combine two workflows. The output workflow triggers when either input triggers
combineWorkflows
  :: (Functor m, Reflex t)
  => (forall wx wy. (wx, wy) -> (wx, wy) -> These wx wy -> (wx, wy))
  -- ^ Choose the resulting workflows among original, current, and ocurring workflows
  -> (forall x y. (m x, m y) -> m (x,y))
  -- ^ Compute resulting widget
  -> ((a,b) -> These () () -> c)
  -- ^ Compute resulting payload based on current payloads and ocurrences
  -> Workflow t m a
  -> Workflow t m b
  -> Workflow t m c
combineWorkflows triggerWorflows combineWidgets combineOccurrence wa0 wb0 = do
  go (These () ()) (wa0, wb0)
  where
    go occurring (wa, wb) = Workflow $ ffor (combineWidgets (unWorkflow wa, unWorkflow wb)) $ \((a0, waEv), (b0, wbEv)) ->
      ( combineOccurrence (a0, b0) occurring
      , ffor (align waEv wbEv) $ \tw ->
          go (tw & here .~ () & there .~ ()) (triggerWorflows (wa0, wb0) (wa, wb) tw)
      )

--------------------------------------------------------------------------------
-- Flattening workflows
--------------------------------------------------------------------------------
-- | Combine two workflows via `combineWorkflows`. Triggers of the first workflow reset the second one.
chainWorkflows
  :: (Functor m, Reflex t)
  => (forall x y. (m x, m y) -> m (x,y)) -- ^ Widget combining function
  -> ((a,b) -> These () () -> c) -- ^ Payload combining function based on ocurring workflow
  -> Workflow t m a
  -> Workflow t m b
  -> Workflow t m c
chainWorkflows = combineWorkflows $ \(_, wb0) (wa, _) -> \case
  This wa' -> (wa', wb0)
  That wb' -> (wa, wb')
  These wa' _ -> (wa', wb0)
