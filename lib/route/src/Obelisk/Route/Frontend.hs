{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Obelisk.Route.Frontend
  ( module Obelisk.Route
  , pattern (:~)
  , Routed
  , RoutedT
  , runRoutedT
  , askRoute
  , withRoutedT
  , mapRoutedT
  , subRoute
  , subRoute_
  , maybeRoute
  , maybeRoute_
  , maybeRouted
  , eitherRoute
  , eitherRoute_
  , eitherRouted
  , runRouteViewT
  , SetRouteT(..)
  , SetRoute(..)
  , runSetRouteT
  , mapSetRouteT
  , RouteToUrl(..)
  , RouteToUrlT(..)
  , runRouteToUrlT
  , mapRouteToUrlT
  , routeLink
  ) where

import Prelude hiding ((.), id)

import Obelisk.Route

import Control.Category (Category (..), (.))
import Control.Category.Cartesian
import Control.Lens hiding (Bifunctor, bimap, index, universe, element)
import Control.Monad ((<=<))
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare
import Data.Monoid
import Data.Proxy
import Data.Functor.Rep
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Universe
import Data.Functor.Compose
import GHC.TypeNats
import Generics.SOP (Code, Generic)
import Reflex.Class
import Reflex.Host.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
import Reflex.PerformEvent.Class
import Reflex.EventWriter.Class
import Reflex.EventWriter.Base
import Reflex.Dynamic
import Reflex.Dom.Builder.Class
import Data.Type.Coercion
import Language.Javascript.JSaddle --TODO: Get rid of this - other platforms can also be routed
import Reflex.Dom.Core hiding (wrap)
import qualified GHCJS.DOM.Types as DOM
import Network.URI
#if defined(ios_HOST_OS)
import Data.Maybe (fromMaybe)
import qualified Data.List as L
#endif

import Unsafe.Coerce

infixr 5 :~
pattern (:~) :: Reflex t => f a -> Dynamic t a -> DSum f (Compose (Dynamic t) Identity)
pattern a :~ b <- a :=> (coerceDynamic . getCompose -> b)

class Routed t r m | m -> t r where
  askRoute :: m (Dynamic t r)
  default askRoute :: (Monad m', MonadTrans f, Routed t r m', m ~ f m') => m (Dynamic t r)
  askRoute = lift askRoute

instance Monad m => Routed t r (RoutedT t r m) where
  askRoute = RoutedT ask

instance (Monad m, Routed t r m) => Routed t r (ReaderT r' m)

newtype RoutedT t r m a = RoutedT { unRoutedT :: ReaderT (Dynamic t r) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadIO, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

instance MonadReader r' m => MonadReader r' (RoutedT t r m) where
  ask = lift ask
  local = mapRoutedT . local

instance HasJSContext m => HasJSContext (RoutedT t r m) where
  type JSContextPhantom (RoutedT t r m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (Prerender js t m, Monad m) => Prerender js t (RoutedT t r m) where
  type Client (RoutedT t r m) = RoutedT t r (Client m)
  prerender server client = RoutedT $ do
    r <- ask
    lift $ prerender (runRoutedT server r) (runRoutedT client r)

instance Requester t m => Requester t (RoutedT t r m) where
  type Request (RoutedT t r m) = Request m
  type Response (RoutedT t r m) = Response m
  requesting = RoutedT . requesting
  requesting_ = RoutedT . requesting_

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (RoutedT t r m)
#endif

instance PerformEvent t m => PerformEvent t (RoutedT t r m) where
  type Performable (RoutedT t r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (RoutedT t r m) where
  type Ref (RoutedT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasJS x m => HasJS x (RoutedT t r m) where
  type JSX (RoutedT t r m) = JSX m
  liftJS = lift . liftJS

deriving instance EventWriter t w m => EventWriter t w (RoutedT t r m)

instance MonadTransControl (RoutedT t r) where
  type StT (RoutedT t r) a = StT (ReaderT (Dynamic t r)) a
  liftWith = defaultLiftWith RoutedT unRoutedT
  restoreT = defaultRestoreT RoutedT

instance PrimMonad m => PrimMonad (RoutedT t r m ) where
  type PrimState (RoutedT t r m) = PrimState m
  primitive = lift . primitive

instance DomBuilder t m => DomBuilder t (RoutedT t r m) where
  type DomBuilderSpace (RoutedT t r m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (RoutedT t r m) where
  runWithReplace a0 a' = RoutedT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = RoutedT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = RoutedT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = RoutedT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (RoutedT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance (Monad m, RouteToUrl r m) => RouteToUrl r (QueryT t q m)

instance (Monad m, SetRoute t r m) => SetRoute t r (QueryT t q m)

instance (Monad m, RouteToUrl r m) => RouteToUrl r (EventWriterT t w m)

instance (Monad m, SetRoute t r m) => SetRoute t r (EventWriterT t w m)

runRoutedT :: RoutedT t r m a -> Dynamic t r -> m a
runRoutedT = runReaderT . unRoutedT

mapRoutedT :: (m a -> n b) -> RoutedT t r m a -> RoutedT t r n b
mapRoutedT f = RoutedT . mapReaderT f . unRoutedT

withRoutedT :: (Dynamic t r -> Dynamic t r') -> RoutedT t r' m a -> RoutedT t r m a
withRoutedT f = RoutedT . withReaderT f . unRoutedT

subRoute_ :: (MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m ()) -> RoutedT t (R r) m ()
subRoute_ f = factorRouted $ strictDynWidget_ $ \(c :=> r') -> do
  runRoutedT (f c) r'

subRoute :: (MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m b) -> RoutedT t (R r) m (Dynamic t b)
subRoute f = factorRouted $ strictDynWidget $ \(c :=> r') -> do
  runRoutedT (f c) r'

maybeRoute_ :: (MonadFix m, MonadHold t m, Adjustable t m) => m () -> RoutedT t r m () -> RoutedT t (Maybe r) m ()
maybeRoute_ n j = maybeRouted $ strictDynWidget_ $ \case
  Nothing -> n
  Just r -> runRoutedT j r

maybeRoute :: (MonadFix m, MonadHold t m, Adjustable t m) => m a -> RoutedT t r m a -> RoutedT t (Maybe r) m (Dynamic t a)
maybeRoute n j = maybeRouted $ strictDynWidget $ \case
  Nothing -> n
  Just r -> runRoutedT j r

{-
maybeRoute :: (MonadFix m, MonadHold t m, GEq r, Adjustable t m) => m a -> RoutedT t r m a -> RoutedT t (Maybe r) m a
maybeRoute f = factorRouted $ strictDynWidget $ \(c :=> r') -> do
  runRoutedT (f c) r'
-}

eitherRoute_
  :: (MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t l m ()
  -> RoutedT t r m ()
  -> RoutedT t (Either l r) m ()
eitherRoute_ l r = eitherRouted $ strictDynWidget_ $ either (runRoutedT l) (runRoutedT r)

eitherRoute
  :: (MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t l m a
  -> RoutedT t r m a
  -> RoutedT t (Either l r) m (Dynamic t a)
eitherRoute l r = eitherRouted $ strictDynWidget $ either (runRoutedT l) (runRoutedT r)

dsumValueCoercion :: Coercion f g -> Coercion (DSum k f) (DSum k g)
dsumValueCoercion Coercion = Coercion

dynamicIdentityCoercion :: Coercion (Compose (Dynamic t) Identity) (Dynamic t)
dynamicIdentityCoercion = unsafeCoerce (Coercion :: Coercion (Identity ()) ()) --TODO: Is it possible to prove this?

factorRouted :: (Reflex t, MonadFix m, MonadHold t m, GEq f) => RoutedT t (DSum f (Dynamic t)) m a -> RoutedT t (DSum f Identity) m a
factorRouted r = RoutedT $ ReaderT $ \d -> do
  d' <- factorDyn d
  runRoutedT r $ (coerceWith (dynamicCoercion $ dsumValueCoercion dynamicIdentityCoercion) d')

maybeRouted :: (Reflex t, MonadFix m, MonadHold t m) => RoutedT t (Maybe (Dynamic t a)) m b -> RoutedT t (Maybe a) m b
maybeRouted r = RoutedT $ ReaderT $ \d -> do
  d' <- maybeDyn d
  runRoutedT r d'

eitherRouted :: (Reflex t, MonadFix m, MonadHold t m) => RoutedT t (Either (Dynamic t a) (Dynamic t b)) m c -> RoutedT t (Either a b) m c
eitherRouted r = RoutedT $ ReaderT $ runRoutedT r <=< eitherDyn

-- | WARNING: The input 'Dynamic' must be fully constructed when this is run
strictDynWidget :: (MonadSample t m, MonadHold t m, Adjustable t m) => (a -> m b) -> RoutedT t a m (Dynamic t b)
strictDynWidget f = RoutedT $ ReaderT $ \r -> do
  r0 <- sample $ current r
  (result0, result') <- runWithReplace (f r0) $ f <$> updated r
  holdDyn result0 result'

strictDynWidget_ :: (MonadSample t m, MonadHold t m, Adjustable t m) => (a -> m ()) -> RoutedT t a m ()
strictDynWidget_ f = RoutedT $ ReaderT $ \r -> do
  r0 <- sample $ current r
  (_, _) <- runWithReplace (f r0) $ f <$> updated r
  pure ()

newtype SetRouteT t r m a = SetRouteT { unSetRouteT :: EventWriterT t (Endo r) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadIO, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

instance (MonadFix m, MonadHold t m, DomBuilder t m) => DomBuilder t (SetRouteT t r m) where
  type DomBuilderSpace (SetRouteT t r m) = DomBuilderSpace m
  element t cfg child = SetRouteT $ element t cfg $ unSetRouteT child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg child = SetRouteT $ selectElement cfg $ unSetRouteT child

instance HasJSContext m => HasJSContext (SetRouteT t r m) where
  type JSContextPhantom (SetRouteT t r m) = JSContextPhantom m
  askJSContext = lift askJSContext

mapSetRouteT :: (forall x. m x -> n x) -> SetRouteT t r m a -> SetRouteT t r n a
mapSetRouteT f (SetRouteT x) = SetRouteT (mapEventWriterT f x)

runSetRouteT :: (Reflex t, Monad m) => SetRouteT t r m a -> m (a, Event t (Endo r))
runSetRouteT = runEventWriterT . unSetRouteT

class Reflex t => SetRoute t r m | m -> t r where
  setRoute :: Event t r -> m ()
  modifyRoute :: Event t (r -> r) -> m ()
  default modifyRoute :: (Monad m', MonadTrans f, SetRoute t r m', m ~ f m') => Event t (r -> r) -> m ()
  modifyRoute = lift . modifyRoute

  setRoute = modifyRoute . fmap const

instance (Reflex t, Monad m) => SetRoute t r (SetRouteT t r m) where
  modifyRoute = SetRouteT . tellEvent . fmap Endo

instance (Monad m, SetRoute t r m) => SetRoute t r (RoutedT t r' m)

instance (Monad m, SetRoute t r m) => SetRoute t r (ReaderT r' m)

instance (PerformEvent t m, Prerender js t m, Monad m, Reflex t) => Prerender js t (SetRouteT t r m) where
  type Client (SetRouteT t r m) = SetRouteT t r (Client m)
  prerender server client = do
    d <- lift $ prerender (runSetRouteT server) (runSetRouteT client)
    let (a, r) = splitDynPure d
    -- Must be prompt here
    SetRouteT . tellEvent $ switchPromptlyDyn r
    pure a

instance Requester t m => Requester t (SetRouteT t r m) where
  type Request (SetRouteT t r m) = Request m
  type Response (SetRouteT t r m) = Response m
  requesting = SetRouteT . requesting
  requesting_ = SetRouteT . requesting_

instance (Monad m, SetRoute t r m) => SetRoute t r (RequesterT t req rsp m)

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (SetRouteT t r m)
#endif

instance PerformEvent t m => PerformEvent t (SetRouteT t r m) where
  type Performable (SetRouteT t r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (SetRouteT t r m) where
  type Ref (SetRouteT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasJS x m => HasJS x (SetRouteT t r m) where
  type JSX (SetRouteT t r m) = JSX m
  liftJS = lift . liftJS

instance PrimMonad m => PrimMonad (SetRouteT t r m ) where
  type PrimState (SetRouteT t r m) = PrimState m
  primitive = lift . primitive

instance (MonadHold t m, Adjustable t m) => Adjustable t (SetRouteT t r m) where
  runWithReplace a0 a' = SetRouteT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (SetRouteT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

class RouteToUrl r m | m -> r where
  askRouteToUrl :: m (r -> Text)
  default askRouteToUrl :: (Monad m', MonadTrans f, RouteToUrl r m', m ~ f m') => m (r -> Text)
  askRouteToUrl = lift askRouteToUrl

newtype RouteToUrlT r m a = RouteToUrlT { unRouteToUrlT :: ReaderT (r -> Text) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadIO, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

runRouteToUrlT
  :: RouteToUrlT r m a
  -> (r -> Text)
  -> m a
runRouteToUrlT a = runReaderT (unRouteToUrlT a)

mapRouteToUrlT :: (forall x. m x -> n x) -> RouteToUrlT r m a -> RouteToUrlT r n a
mapRouteToUrlT f (RouteToUrlT m) = RouteToUrlT $ mapReaderT f m

instance Monad m => RouteToUrl r (RouteToUrlT r m) where
  askRouteToUrl = RouteToUrlT ask

instance (Monad m, RouteToUrl r m) => RouteToUrl r (SetRouteT t r' m) where

instance (Monad m, RouteToUrl r m) => RouteToUrl r (RoutedT t r' m) where

instance (Monad m, RouteToUrl r m) => RouteToUrl r (ReaderT r' m) where

instance (Monad m, RouteToUrl r m) => RouteToUrl r (RequesterT t req rsp m)

instance HasJSContext m => HasJSContext (RouteToUrlT r m) where
  type JSContextPhantom (RouteToUrlT r m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (Prerender js t m, Monad m) => Prerender js t (RouteToUrlT r m) where
  type Client (RouteToUrlT r m) = RouteToUrlT r (Client m)
  prerender server client = do
    r <- RouteToUrlT ask
    lift $ prerender (runRouteToUrlT server r) (runRouteToUrlT client r)

instance Requester t m => Requester t (RouteToUrlT r m) where
  type Request (RouteToUrlT r m) = Request m
  type Response (RouteToUrlT r m) = Response m
  requesting = RouteToUrlT . requesting
  requesting_ = RouteToUrlT . requesting_

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (RouteToUrlT r m)
#endif

instance PerformEvent t m => PerformEvent t (RouteToUrlT r m) where
  type Performable (RouteToUrlT r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (RouteToUrlT r m) where
  type Ref (RouteToUrlT r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasJS x m => HasJS x (RouteToUrlT r m) where
  type JSX (RouteToUrlT r m) = JSX m
  liftJS = lift . liftJS

instance MonadTransControl (RouteToUrlT r) where
  type StT (RouteToUrlT r) a = StT (ReaderT (r -> Text)) a
  liftWith = defaultLiftWith RouteToUrlT unRouteToUrlT
  restoreT = defaultRestoreT RouteToUrlT

instance PrimMonad m => PrimMonad (RouteToUrlT r m ) where
  type PrimState (RouteToUrlT r m) = PrimState m
  primitive = lift . primitive

instance DomBuilder t m => DomBuilder t (RouteToUrlT r m) where
  type DomBuilderSpace (RouteToUrlT r m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (RouteToUrlT r m) where
  runWithReplace a0 a' = RouteToUrlT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = RouteToUrlT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = RouteToUrlT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = RouteToUrlT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (RouteToUrlT r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

runRouteViewT
  :: forall t m r a.
     ( TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     )
  => (Encoder Identity Identity r PageName)
  -> Event t () -- ^ Switchover event, nothing is done until this event fires. Used to prevent incorrect DOM expectations at hydration switchover time
  -> RoutedT t r (SetRouteT t r (RouteToUrlT r m)) a
  -> m a
runRouteViewT routeEncoder switchover a = do
  rec historyState <- manageHistory' switchover $ HistoryCommand_PushState <$> setState
      let theEncoder = pageNameEncoder . hoistParse (pure . runIdentity) routeEncoder
          -- NB: The can only fail if the uriPath doesn't begin with a '/' or if the uriQuery
          -- is nonempty, but begins with a character that isn't '?'. Since we don't expect
          -- this ever to happen, we'll just handle it by failing completely with 'error'.
          route :: Dynamic t r
          route = fmap (errorLeft . tryDecode theEncoder . (adaptedUriPath &&& uriQuery) . _historyItem_uri) historyState
            where
              errorLeft (Left e) = error (T.unpack e)
              errorLeft (Right x) = x
      (result, changeState) <- runRouteToUrlT (runSetRouteT $ runRoutedT a route) $ (\(p, q) -> T.pack $ p <> q) . encode theEncoder
      let f (currentHistoryState, oldRoute) change =
            let newRoute = appEndo change oldRoute
                (newPath, newQuery) = encode theEncoder newRoute
            in HistoryStateUpdate
               { _historyStateUpdate_state = DOM.SerializedScriptValue jsNull
                 -- We always provide "" as the title.  On Firefox, Chrome, and
                 -- Edge, this parameter does nothing.  On Safari, "" has the
                 -- same behavior as other browsers (as far as I can tell), but
                 -- anything else sets the title for the back button list item
                 -- the *next* time pushState is called, unless the page title
                 -- is changed in the interim.  Since the Safari functionality
                 -- is near-pointless and also confusing, I'm not going to even
                 -- bother exposing it; if there ends up being a real use case,
                 -- we can change this function later to accommodate.
                 -- See: https://github.com/whatwg/html/issues/2174
               , _historyStateUpdate_title = ""
               , _historyStateUpdate_uri = Just $ setAdaptedUriPath newPath $ (_historyItem_uri currentHistoryState)
                 { uriQuery = newQuery
                 }
               }
          setState = attachWith f ((,) <$> current historyState <*> current route) changeState
  return result

-- | A link widget that, when clicked, sets the route to the provided route. In non-javascript
-- contexts, this widget falls back to using @href@s to control navigation
routeLink
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     )
  => R route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
routeLink r w = do
  enc <- askRouteToUrl
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ "href" =: enc r
  (e, a) <- element "a" cfg w
  setRoute $ r <$ domEvent Click e
  return a

-- On ios due to sandboxing when loading the page from a file adapt the
-- path to be based on the hash.

adaptedUriPath :: URI -> String
#if defined(ios_HOST_OS)
adaptedUriPath = hashToPath . uriFragment

hashToPath :: String -> String
hashToPath = ('/' :) . fromMaybe "" . L.stripPrefix "#"
#else
adaptedUriPath = uriPath
#endif

setAdaptedUriPath :: String -> URI -> URI
#if defined(ios_HOST_OS)
setAdaptedUriPath s u = u { uriFragment = pathToHash s }

pathToHash :: String -> String
pathToHash = ('#' :) . fromMaybe "" . L.stripPrefix "/"
#else
setAdaptedUriPath s u = u { uriPath = s }
#endif






instance Generic (These a b)


{- From https://github.com/reflex-frp/reflex/pull/106 -}

type family FunctorWrapTypeList (f :: * -> *) (xs :: [*]) :: [*] where
  FunctorWrapTypeList f '[] = '[]
  FunctorWrapTypeList f (x ': xs) = f x ': FunctorWrapTypeList f xs

type family FunctorWrapTypeListOfLists (f :: * -> *) (xss :: [[*]]) :: [[*]] where
  FunctorWrapTypeListOfLists f '[] = '[]
  FunctorWrapTypeListOfLists f (xs ': xsTail) = FunctorWrapTypeList f xs ': FunctorWrapTypeListOfLists f xsTail

type WrapsDyn t a' a = (Generic a', Generic a, Code a' ~ FunctorWrapTypeListOfLists (Dynamic t) (Code a))

factorDynGeneric
  :: forall t m a a'.
   ( Reflex t, MonadFix m, MonadHold t m
   , WrapsDyn t a' a)
  => Dynamic t a -> m (Dynamic t a')
factorDynGeneric = undefined


{- route combinators -}

genericRouted :: (Reflex t, MonadFix m, MonadHold t m, WrapsDyn t r' r) => RoutedT t r' m a -> RoutedT t r m a
genericRouted r = RoutedT $ ReaderT $ runRoutedT r <=< factorDynGeneric

genericRoute
  :: (MonadFix m, MonadHold t m, Adjustable t m, WrapsDyn t r' r)
  => (r' -> m a)
  -> RoutedT t r m (Dynamic t a)
genericRoute = genericRouted . strictDynWidget

maybeRoute' :: (MonadFix m, MonadHold t m, Adjustable t m) => m a -> RoutedT t r m a -> RoutedT t (Maybe r) m (Dynamic t a)
maybeRoute' n j = genericRoute $ maybe n (runRoutedT j)

eitherRoute'
  :: (MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t x m a
  -> RoutedT t y m a
  -> RoutedT t (Either x y) m (Dynamic t a)
eitherRoute' x y = genericRoute $ either (runRoutedT x) (runRoutedT y)

theseRoute'
  :: (MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t x m a
  -> RoutedT t y m a
  -> RoutedT t (x,y) m a
  -> RoutedT t (These x y) m (Dynamic t a)
theseRoute' x y z = genericRoute $ these (runRoutedT x) (runRoutedT y) (\x y -> runRoutedT z $ zipDyn x y)


{- dyn combinators -}

dynGeneric_
  :: forall t m a b.
   ( Reflex t, MonadFix m, MonadHold t m
   , WrapsDyn t b a
   , DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t a
  -> (b -> m ())
  -> m ()
dynGeneric_ dma k = void $ dynGeneric dma k

dynGeneric
  :: forall t m a a' b.
   ( Reflex t, MonadFix m, MonadHold t m
   , WrapsDyn t a' a
   , DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t a
  -> (a' -> m b)
  -> m (Event t b)
dynGeneric dma k = dyn . fmap k =<< factorDynGeneric dma


dynMaybe
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m z
  -> (Dynamic t a -> m z)
  -> Dynamic t (Maybe a)
  -> m (Event t z)
dynMaybe nothing' just' d = dynGeneric d $ maybe nothing' just'

dynMaybe_
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m ()
  -> (Dynamic t a -> m ())
  -> Dynamic t (Maybe a)
  -> m ()
dynMaybe_ nothing' just' d = dynGeneric_ d $ maybe nothing' just'


dynEither
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => (Dynamic t a -> m z)
  -> (Dynamic t b -> m z)
  -> Dynamic t (Either a b)
  -> m (Event t z)
dynEither left' right' d = dynGeneric d $ either left' right'

dynEither_
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => (Dynamic t a -> m ())
  -> (Dynamic t b -> m ())
  -> Dynamic t (Either a b)
  -> m ()
dynEither_ left' right' d = dynGeneric_ d $ either left' right'


dynThese
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => (Dynamic t a -> m z)
  -> (Dynamic t b -> m z)
  -> (Dynamic t a -> Dynamic t b -> m z)
  -> Dynamic t (These a b)
  -> m (Event t z)
dynThese this' that' these' d = dynGeneric d $ these this' that' these'


dynRequest_
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m ()
  -> (Dynamic t e -> m ())
  -> m ()
  -> (Dynamic t a -> m ())
  -> Dynamic t (Maybe (Either e (Maybe a)))
  -> m ()
dynRequest_ loading failure missing success = dynMaybe_ loading $ dynEither_ failure $ dynMaybe_ missing success
















class Representable' prod where
  type Rep' prod :: Nat -> *
  tabulate' :: (forall n. Rep' prod k -> F prod n) -> prod
  index' :: prod -> (forall n. Rep' prod n -> F prod n)

type family F p (k :: Nat) where
  F (Record a b c) 0 = a
  F (Record a b c) 1 = b
  F (Record a b c) 2 = c

data Record a b c = Record
  { _a :: a
  , _b :: b
  , _c :: c
  }

data RecordTag a b c (n :: Nat) where
  RecordTag_A :: RecordTag a b c 0
  RecordTag_B :: RecordTag a b c 1
  RecordTag_C :: RecordTag a b c 2

instance Representable' (Record a b c) where
  type Rep' (Record a b c) = RecordTag a b c

  tabulate' f = Record
    { _a = f RecordTag_A
    , _b = f RecordTag_B
    , _c = f RecordTag_C
    }

  index' r = \case
    RecordTag_A -> _a r
    RecordTag_B -> _b r
    RecordTag_C -> _c r

notCoerce :: RecordTag a b c n -> RecordTag a' b' c' n
notCoerce = \case
  RecordTag_A -> RecordTag_A
  RecordTag_B -> RecordTag_B
  RecordTag_C -> RecordTag_C

wrap :: (forall x. x -> f x) -> Record a b c -> Record (f a) (f b) (f c)
wrap w r = tabulate' $ \case
  RecordTag_A -> w $ index' r RecordTag_A
  RecordTag_B -> w $ index' r RecordTag_B
  RecordTag_C -> w $ index' r RecordTag_C

nothings = wrap (const Nothing)
justs = wrap Just

{-
wrap' :: (forall x. x -> f x) -> Record a b c -> Record (f a) (f b) (f c)
wrap' w r = tabulate' $ \t -> w $ index' r (notCoerce t)

    • Couldn't match type ‘F prod n0’ with ‘F prod n’
      Expected type: Rep' prod k -> F prod n
        Actual type: Rep' prod k -> F prod n0
      NB: ‘F’ is a non-injective type family
      The type variable ‘n0’ is ambiguous
-}

fmap' :: (forall k. F (Record a b c) k -> F (Record a' b' c') k) -> Record a b c -> Record a' b' c'
fmap' f r = tabulate' $ \case
  RecordTag_A -> f $ index' r RecordTag_A
  RecordTag_B -> f $ index' r RecordTag_B
  RecordTag_C -> f $ index' r RecordTag_C

{-
    • Couldn't match type ‘F (Record (f a) (f b) (f c)) k0’
                     with ‘F (Record (f a) (f b) (f c)) k’
      Expected type: F (Record a b c) k -> F (Record (f a) (f b) (f c)) k
        Actual type: F (Record a b c) k0
                     -> F (Record (f a) (f b) (f c)) k0
      NB: ‘F’ is a non-injective type family
-}
--nothings = fmap' (const Nothing)
--justs = fmap' Just

--(\proj tag -> id (proj tag))
--nothings :: Record a b c f -> Record a b c Maybe
--fmap' :: (forall x. f x -> g x) -> Record a b c f -> Record a b c g
--fmap' nt r = fmapRep' (\proj tag -> proj tag) r

{-
fmapRep' :: forall a b. (Representable' a, Representable' b, Coercible (Rep' a) (Rep' b))
--         => ((forall x. Rep' a x -> x -> x))
         => ((forall k. (Rep' a k -> F prod k)) -> (forall x. (Rep' b x -> x)))
         -> a
         -> b
fmapRep' f a = tabulate' $ f $ index' a
-}
{-
--Universe (Some.Some f)
sequence' :: (Representable f, _) => f (g a) -> g (f a)
sequence' fg = do
  let frep = index fg
      un = fmap (\v -> fmap (v,) (frep v)) universe
      wut = sequence un :: Int

  undefined
-}

--liftR2' :: Record a b c -> Record a' b' c' -> Record (a, a') (b, b') (c c')
--liftR2 :: asdf dsa asdf dsa asdf dsa asdf fdsa asdf fdsa asdf dsa
{-
instance Universe (Some.Some (RecordTag a b c)) where
  universe = [Some.This RecordTag_A, Some.This RecordTag_B, Some.This RecordTag_C]
-}

{-
dup :: Record a b -> Record (a,a) (b,b)
dup r = fmapRep' (\f t -> case t of
                     RecordTag_A -> (f t, f t)
                     RecordTag_B -> (f t, f t)
                     RecordTag_C -> (f t, f t)
                 ) r
-}

--fmapRep' :: Record a b -> Record ) => (forall x. Rep' f x -> Rep' g x) -> f -> g
--fmapRep' f p = do


--instance (Representable' (f a), Representable' (f b), Representable' (f a) ~ Representable' (f b)) => Representable' (f (a,b)) where
--  type Rep' (f (a,b)) = Rep' (f a)
--  index'
{-
zipRep' :: (Representable' (f a), Representable' (f b), Representable' (f (a,b))
           , Rep' (f a) ~ Rep' (f b)
           , Rep' (f a) ~ Rep' (f (a,b))
           )
        => (f a) -> (f b) -> (f (a,b))
zipRep' a b = tabulate' $ \k -> _ --(index' a k, index' b k)
-}
{-
liftR2' :: Representable' (p a) => p a -> p (a,a)
liftR2' p = tabulate' $ \k -> index' p k

liftR2' :: Representable' (p a) => p a -> p (a,a)
liftR2' p = tabulate' $ \k -> index' p k

liftR2' :: Representable' (p a) => p a -> p (a,a)
liftR2' p = tabulate' $ \k -> index' p k
-}
