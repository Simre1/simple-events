{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimpleEvents
  ( -- *Setup monad
    Setup
  , MonadSetup (..)

  -- * Events & Behaviors
  , Event
  , EventTrigger
  , link
  , Behavior
  , readBehavior
  , Dynamic
  , behaviorFromDynamic
  , eventFromDynamic
  , apply
  , stepEvents
  , accumEvents
  --
  -- ** Filtering
  , filterE
  , mFilterE

  -- ** Switching
  , switchDynamics
  , switchEvents
  -- * Make bindings
  , newEvent
  , customEvent
  , newDynamic
  , reactimate
  , execEventSetup
  , execDynamicSetup
  --, runIOInSetup
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Data.Functor.Contravariant
import Data.IntMap as IM
import Data.IORef

-- The datatype in which to setup the event network. You can execute in IO with 'liftSetup'.
newtype Setup a = Setup {runSetup :: IO a} deriving (Functor, Applicative, Monad, Semigroup, Monoid, Alternative, MonadPlus)

class MonadSetup m where
  liftSetup :: Setup a -> m a

instance MonadSetup Setup where
  liftSetup = id

instance MonadSetup IO where
  liftSetup = runSetup


newtype Event a = Event {registerHandler :: (a -> IO ()) -> Setup (Setup ())}

instance Functor Event where
  fmap f (Event reg) = Event $ \a -> reg (a . f)

instance Semigroup (Event a) where
  (Event r1) <> (Event r2) = Event $ \handler -> do
    unR1 <- r1 handler
    unR2 <- r2 handler
    pure (unR1 *> unR2)

instance Monoid (Event a) where
  mempty = Event $ const (pure (pure ()))

newtype Behavior a = Behavior {behaviorValue :: Setup a} deriving (Functor, Applicative, Monad, Semigroup, Monoid)

data Dynamic a = Dynamic {dBehavior :: Behavior a, dEvent :: Event a}

instance Functor Dynamic where
  fmap f (Dynamic a e) = Dynamic (f <$> a) (f <$> e)

instance Semigroup a => Semigroup (Dynamic a) where
  d1 <> d2 = (<>) <$> d1 <*> d2


instance Monoid a => Monoid (Dynamic a) where
  mempty = Dynamic mempty mempty

instance Applicative Dynamic where
  pure a = Dynamic (pure a) mempty
  (Dynamic acc1 (Event reg1)) <*> (Dynamic acc2 (Event reg2)) = Dynamic (acc1 <*> acc2) applicativeEvent
    where applicativeEvent = Event $ \f -> do
            unReg1 <- reg1 (\ab -> ab <$> readBehavior acc2 >>= f)
            unReg2 <- reg2 (\a -> (\ab -> ab a) <$> readBehavior acc1 >>= f)
            pure (unReg1 *> unReg2)


newtype EventTrigger a = EventTrigger {trigger :: a -> IO ()}

instance Contravariant EventTrigger where
  contramap f (EventTrigger fire) = EventTrigger (fire . f)

instance Semigroup (EventTrigger a) where
  (EventTrigger f1) <> (EventTrigger f2) = EventTrigger $ \a -> f1 a <> f2 a

instance Monoid (EventTrigger a) where
  mempty = EventTrigger $ const mempty

readBehavior :: MonadSetup m => Behavior a -> m a
readBehavior = liftSetup . behaviorValue

link :: MonadSetup m => Event a -> EventTrigger a -> m (Setup ())
link ev tr = reactimate ev (trigger tr)

stepEvents :: MonadSetup m => Event a -> a -> m (Dynamic a)
stepEvents ev initial = liftSetup $ do
  bIORef <-  runIOInSetup $ newIORef initial
  reactimate ev (liftIO . writeIORef bIORef)
  pure $ Dynamic (Behavior . runIOInSetup $ readIORef bIORef) ev

apply :: Behavior (a -> b) -> Event a -> Event b
apply b (Event reg) = Event $ \action -> reg $ \a -> ($ a) <$> readBehavior b >>= action

accumEvents :: MonadSetup m => Event (a -> a) -> a -> m (Dynamic a)
accumEvents ev initial = liftSetup $ do
  bIORef <-  runIOInSetup $ newIORef initial
  reactimate ev (\f -> (f <$> liftIO (readIORef bIORef)) >>= liftIO . writeIORef bIORef)
  pure $ Dynamic (Behavior . runIOInSetup $ readIORef bIORef) (apply (Behavior $ const <$> runIOInSetup (readIORef bIORef)) ev)

filterE :: (a -> Bool) -> Event a -> Event a
filterE check (Event reg) = Event $ reg . filterAction
  where filterAction action a = if check a then action a else pure ()

mFilterE :: (a -> IO Bool) -> Event a -> Event a
mFilterE check (Event reg) = Event $ reg . filterAction
  where filterAction action a = do
          x <- check a
          if x then action a else pure ()

eventFromDynamic :: Dynamic a -> Event a
eventFromDynamic = dEvent

behaviorFromDynamic :: Dynamic a -> Behavior a
behaviorFromDynamic = dBehavior


switchEvents' :: MonadSetup m => Event (Event a) -> m (Event a)
switchEvents' ev = liftSetup $ do
  (nEvent, fire) <- newEvent
  lastUnReg <- runIOInSetup $ newIORef mempty
  reactimate ev $ \switchingEv -> liftSetup $ do
    unReg2 <- reactimate switchingEv $ trigger fire
    join $ runIOInSetup $ atomicModifyIORef' lastUnReg (\lastUnReg' -> (unReg2, lastUnReg'))
  pure nEvent

switchEvents :: MonadSetup m => Dynamic (Event a) -> m (Event a)
switchEvents (Dynamic acc e) = liftSetup $ do
  firstEvent <- readBehavior acc
  (nEvent, EventTrigger fire) <- newEvent
  lastUnReg <- runIOInSetup $ newIORef mempty
  unReg <- reactimate firstEvent fire
  join . runIOInSetup $ atomicModifyIORef' lastUnReg (\lastUnReg' -> (unReg, lastUnReg'))
  reactimate e $ \switchingEv -> liftSetup $ do
    unReg2 <- reactimate switchingEv fire
    join . runIOInSetup $ atomicModifyIORef' lastUnReg (\lastUnReg' -> (unReg2, lastUnReg'))
  pure nEvent

switchDynamics :: MonadSetup m => Dynamic (Dynamic a) -> m (Dynamic a)
switchDynamics bb@(Dynamic acc e) = liftSetup $ do
  currentValue <- readBehavior acc >>= readBehavior . behaviorFromDynamic
  (b, EventTrigger fire) <- newDynamic currentValue
  innerChanging <- switchEvents (fmap eventFromDynamic bb)
  let outerChanging = e
  reactimate innerChanging fire
  reactimate outerChanging (\oB -> readBehavior (behaviorFromDynamic oB) >>= fire)
  pure b

execEventSetup :: Event (Setup a) -> Event a
execEventSetup (Event regSA) = Event $ regSA . execSetup
  where
    execSetup f setup = liftSetup setup >>= f

execDynamicSetup :: Dynamic (Setup a) -> Dynamic a
execDynamicSetup (Dynamic acc e) = Dynamic (execBehaviorSetup acc) (execEventSetup e)
  where
    execBehaviorSetup (Behavior val) = Behavior $ join val


reactimate :: MonadSetup m => Event a -> (a -> IO ()) -> m (Setup ())
reactimate = fmap liftSetup <$> registerHandler

newEvent :: MonadSetup m => m (Event a, EventTrigger a)
newEvent = liftSetup $ do
  ref <- runIOInSetup $ newIORef (IM.empty, 0)
  let onChange action = runIOInSetup $ do
            eventHandlerInc <- atomicModifyIORef' ref (\(fl, inc) -> ((insert inc action fl, succ inc), inc))
            pure . runIOInSetup $ modifyIORef' ref $ first (delete eventHandlerInc)
      fire val = IM.foldl' (\a b -> a *> b val) (pure ()) . fst =<< liftIO (readIORef ref)
  return (Event onChange, EventTrigger fire)

customEvent :: MonadSetup m => ((a -> IO ()) -> IO (IO ())) -> (a -> IO ()) -> m (Event a, EventTrigger a)
customEvent reg fire = liftSetup . Setup . pure $ (Event (runIOInSetup . fmap runIOInSetup . reg), EventTrigger fire)

newDynamic :: MonadSetup m => a -> m (Dynamic a, EventTrigger a)
newDynamic i = liftSetup $ do
  (e,f) <- newEvent
  d <- stepEvents e i
  pure (d,f)

-- | Quickly test an event network. The first argument of the function is an action which halts the application and should be used in conjunction with reactimate.
quickSetup :: (IO () -> IO ()) -> IO ()
quickSetup setup = do
  ref <- newEmptyMVar
  setup (putMVar ref ())
  takeMVar ref

runIOInSetup :: IO a -> Setup a
runIOInSetup = Setup
