{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Frontend where

import Control.Lens (FunctorWithIndex(..), set)
import Control.Monad (ap, (<=<))
import Control.Monad.Fix
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Comonad (Comonad, duplicate, extract)
import Control.Comonad.Cofree
import Data.Align
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.These
import Data.Tuple (swap)
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (wrap)

import Common.Route

--------------------------------------------------------------------------------
-- Workflow monad
--------------------------------------------------------------------------------
type Step t m = Compose m (Event t)
newtype W t m a = W { unW :: F (Step t m) a } deriving (Functor, Applicative, Monad)
newtype W' (t :: *) m a = W' { unW' :: m (Event t a, Event t (W' t m a)) } deriving (Functor)
type WInternal' t m = Compose m (Event t)

{-
instance (Reflex t, Functor m) => Comonad (W' t m) where
  extract (W' w) = extract w
  duplicate (W' w) = W' $ fmap W' $ duplicate w
-}
wtf :: Monad m => m (a -> b) -> m a -> m b
wtf = ap

instance (PostBuild t m) => Applicative (W' t m) where
  pure a = W' $ ffor getPostBuild $ \pb -> (a <$ pb, never)
  (<*>) = undefined --ap --(<.>)

instance (Reflex t, Functor m) => Apply (W' t m) where
  (<.>) = undefined

instance (Reflex t, Apply m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Bind (W' t m) where
  join ww = W' $ do
    (inner, outer) :: (Event t (W' t m a), Event t (W' t m (W' t m a)))  <- unW' ww
    ev <- runW' inner
    pure (ev, never)
{-
    o <- outer
    i <- runW' innerW

    let flat = fmap (unW' . join . W') o
    pure flat
-}
instance (Reflex t, Apply m, Applicative m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => Monad (W' t m) where
  (>>=) = (>>-)

runW :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => W t m a -> m (Event t a)
runW (W w0) = do
  let go :: F (Step t m) a -> m (Event t (F (Step t m) a))
      go w = runF w
        (\l -> (return l <$) <$> getPostBuild) --TODO: Can this just be blank?
        (\(Compose r) -> fmap (fmapCheap (wrap . Compose)) r)
  rec (next0, built) <- runWithReplace (go w0) $ go <$> next
      next <- switch <$> hold next0 built
  return $ fmapMaybe (\w -> runF w Just (const Nothing)) next

runW' :: forall t m a. (Apply m, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => W' t m a -> m (Event t a)
runW' w0 = mdo
  ((aEv0, next0), built) <- runWithReplace (unW' w0) (fmap unW' next)
  next <- switchHold next0 $ fmap snd built
  switchHold aEv0 $ fmap fst built

prompt :: (Reflex t, Functor m) => m (Event t a) -> W t m a
prompt = W . wrap . fmap return . Compose

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      clk <- button "simultaneous"
      br
      let
        w2 = wn clk 2 0
        w3 = wn clk 3 0

      workflow w2 >>= display
      br
      workflow w3 >>= display
      br

      renderW "<>" $ fmap show w2 <> pure " " <> fmap show w3
      renderW "<.>" $ (,) <$> w2 <*> w3

      renderW "<!>" $ w2 <!> w3
      renderW "join" $ join $ ww clk 5 0

      renderW "<.>" $ independentWorkflows (\(ma, mb) -> do
                                               b <- mb
                                               text " <reversed> "
                                               a <- ma
                                               pure (a, b)) const w2 w3

      br
      c <- count =<< button "outer"
      br
      let wdyn = ffor c $ \(c' :: Int) -> do
            i :: Dynamic t Int <- count =<< button "inner"
            dyn_ $ ffor i $ \i' ->
              text $ tshow i' <> " / " <> tshow c'
      dyn_ wdyn
      br
      s <- button "sample"
      br
      widgetHold_ (text "loading") $ current wdyn <@ s

      br
      br
      text "Free workflows"
      br
      ev <- runW $ do
        a <- fwn clk 5 0
        b <- fwn clk 4 0
        fwn clk 3 0
      br
      display =<< holdDyn Nothing (fmap Just ev)

      br
      br
      text "Cofree workflows"
      ev' <- runW' $ do
        a <- fwn' clk 5 0
        b <- fwn' clk (a + 1) 0
        fwn' clk (b + 1) 0
      br
      display =<< holdDyn Nothing (fmap Just ev')
      br
      display =<< count ev'
      pure ()
  }

tshow :: Show a => a -> T.Text
tshow = T.pack . show

wn :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> Workflow t m Int
wn ev n i = Workflow $ do
  inc <- button $ T.pack $ show i <> "/" <> show n
  c <- count inc
  dyn_ $ ffor c $ \(j :: Int) -> text $ "(state: " <> T.pack (show j) <> ")   "
  pure (i, wn ev n ((i + 1) `mod` n) <$ leftmost [ev, inc])

ww :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> Workflow t m (Workflow t m Int)
ww ev n i = Workflow $ do
  next <- button "next"
  c <- count next
  dyn_ $ ffor c $ \(j :: Int) -> text $ "(state: " <> T.pack (show j) <> ")   "
  pure (wn ev (i + 1) 0, ww ev n ((i + 1) `mod` n) <$ leftmost [ev, next])


fwn :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> W t m Int
fwn ev n i = W $ toF $ Free $ Compose $ (fmap . fmap) (fromF . unW) $ do
  inc <- button $ T.pack $ show i <> "/" <> show n
  pure $ (fwn ev n ((i + 1) `mod` n)) <$ leftmost [ev, inc]
{-
fwn ev n i = do
  pure n
-}

{-
fww :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => Event t () -> Int -> Int -> W t m Int
fww ev n i = W $ toF $ Free $ Compose $ do
  next <- button "next"
  pure $ (fromF . unW) (fwn ev (i + 1) 0, fww ev n ((i + 1) `mod` n)) <$ leftmost [ev, next]
-}

--mkWorkflow :: (Reflex t, Functor m) => a -> m (Event t (W' t m a)) -> W' t m a
--mkWorkflow a ev = W' (a :< Compose ((fmap . fmap) unW' ev))

mkWorkflow :: (Reflex t, Monad m, PostBuild t m) => a -> m (Event t (W' t m a)) -> W' t m a
mkWorkflow a m = W' $ do
  pb <- getPostBuild
  ev <- m
  pure (a <$ pb, ev)

fwn' :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Event t () -> Int -> Int -> W' t m Int
fwn' ev n i = mkWorkflow i $ do
  br
  inc <- button $ T.pack $ show i <> "/" <> show n
  c <- count inc
  dyn_ $ ffor c $ \(j :: Int) -> text $ "(state: " <> T.pack (show j) <> ")   "
  pure $ fwn' ev n ((i + 1) `mod` n) <$ leftmost [ev, inc]

br :: DomBuilder t m => m ()
br = el "br" blank

renderW :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Show a) => T.Text -> Workflow t m a -> m ()
renderW lbl w = do
  text lbl
  ipayload <- if True
              then workflow' $ imap (,) w
              else uncurry holdDyn <=< workflowView' $ imap (,) w
  dynText $ ffor ipayload $ \(k, p) -> "[" <> tshow k <> "] " <> tshow p
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
