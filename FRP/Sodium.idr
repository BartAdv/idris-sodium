module FRP.Sodium

import Data.IORef
import FRP.Sodium.Reactive
import FRP.Sodium.Vertex

record Listener a where
  constructor MkListener
  handler : (a -> Reactive ())
  targetID  : VertexID

Eq (Listener a) where

||| The internal data for an event to manage the listeners
-- naming convention taken from Reflex
record EventListened a where
  constructor MkEventListened
  vertex : VertexRef
  listeners : IORef (List (Listener a))
  firings : IORef (List a)

record Event a where
  constructor MkEvent
  eventListenedRef : IORef (Maybe (EventListened a))

-- we use the same trick as Haskell implementation to avoid problems with the unsafe
-- call being optimized away, although I'm not even sure if Idris does this
unsafeNewIORef : a -> b -> IORef a
unsafeNewIORef v dummy = unsafePerformIO (newIORef v)

newEventListened : String -> Reactive (EventListened a)
-- newEventListened name = do
--   vertex <- newVertex name

getEventListened : Event a -> Reactive (EventListened a)
getEventListened (MkEvent cacheRef) = do
  Nothing <- lift $ readIORef cacheRef | Just l => pure l
  l <- newEventListened "temp"
  lift $ writeIORef cacheRef (Just l)
  pure l

listen_ : Event a -> VertexRef -> (a -> Reactive ()) -> Bool -> Reactive (Reactive ())
listen_ ev target h suppressEarlierFirings = do
    (MkEventListened vertex listeners firings) <- getEventListened ev
    when !(lift $ registerVertex vertex target) requestRegen
    let listener = MkListener h (vertexID !(lift $ readIORef target))
    lift $ modifyIORef listeners (listener::)
    when (not suppressEarlierFirings) $ traverse_ h !(lift $ readIORef firings) -- prioritized?
    pure $ unlisten ev listener
  where
    unlisten : Event a -> Listener a -> Reactive ()
    unlisten (MkEvent eventListenedRef) listener = do
      Just (MkEventListened vertex listeners _) <- lift $ readIORef eventListenedRef | Nothing => pure ()
      lift $ do modifyIORef listeners (delete listener)
                deregisterVertex vertex target

send_ : Event a -> a -> Reactive ()
send_ ev v = do
 -- TODO: vertex refCount check
 (MkEventListened _ listeners firings) <- getEventListened ev
 when (isNil !(lift $ readIORef firings)) $ scheduleLast $
   lift $ modifyIORef firings (const [])
 lift $ modifyIORef firings (v::)
 traverse_ (\l => (handler l) v) !(lift $ readIORef listeners)

map : (a -> b) -> Event a -> Event b
map f e = MkEvent cacheRef
where cacheRef = unsafeNewIORef Nothing e
