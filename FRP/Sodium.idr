module FRP.Sodium

import Data.IORef
import FRP.Sodium.Reactive
import FRP.Sodium.Vertex

record Listener a where
  constructor MkListener
  handler : (a -> Reactive ())
  targetID  : VertexID

Eq (Listener a) where
  (==) (MkListener _ xid) (MkListener _ yid) = xid == yid
  (/=) (MkListener _ xid) (MkListener _ yid) = xid /= yid

||| The internal data for an event to manage the listeners
-- naming convention taken from Reflex
record EventListened a where
  constructor MkEventListened
  vertex : VertexRef
  listeners : IORef (List (Listener a))
  firings : IORef (List a)

newEventListened : VertexRef -> Reactive (EventListened a)
newEventListened vertex = pure $ MkEventListened vertex !(newRef []) !(newRef [])

data GetEventListened a = CreateEventListened (Reactive (EventListened a))
                        | CachedEventListened (EventListened a)

record Event a where
  constructor MkEvent
  eventListened : IORef (GetEventListened a)

newEvent : Reactive (Event a)
newEvent = do
  vertex <- newVertex "Event" 0 []
  el <- newEventListened vertex
  let ev = MkEvent !(newRef (CachedEventListened el))
  pure ev

-- we use the same trick as Haskell implementation to avoid problems with the unsafe
-- call being optimized away, although I'm not even sure if Idris does this
unsafeNewIORef : a -> b -> IORef a
unsafeNewIORef v dummy = unsafePerformIO (newIORef v)

getEventListened : Event a -> Reactive (EventListened a)
getEventListened (MkEvent eventListenedRef) = do
  CreateEventListened create <- readRef eventListenedRef | CachedEventListened l => pure l
  l <- create
  writeRef eventListenedRef (CachedEventListened l)
  pure l

listen_ : Event a -> VertexRef -> (a -> Reactive ()) -> Bool -> Register
listen_ ev target h suppressEarlierFirings = do
    MkEventListened vertex listeners firings <- getEventListened ev
    when !(registerVertex vertex target) requestRegen
    let listener = MkListener h (vertexID !(readRef target))
    modifyRef listeners (listener::)
    when (not suppressEarlierFirings) $ traverse_ h !(readRef firings) -- prioritized?
    pure $ unlisten ev listener
  where
    unlisten : Event a -> Listener a -> Deregister
    unlisten (MkEvent eventListenedRef) listener = do
      CachedEventListened (MkEventListened vertex listeners _) <- readRef eventListenedRef | CreateEventListened _ => pure ()
      modifyRef listeners (delete listener)
      deregisterVertex vertex target

send_ : Event a -> a -> Reactive ()
send_ ev v = do
 -- TODO: vertex refCount check
 MkEventListened _ listeners firings <- getEventListened ev
 when (isNil !(readRef firings)) $ scheduleLast $
   modifyRef firings (const [])
 modifyRef firings (v::)
 traverse_ (\l => (handler l) v) !(readRef listeners)

map : (a -> b) -> Event a -> Event b
map f e = out
  where
    mutual
      out : Event b
      out = MkEvent eventListenedRef

      create : Reactive (EventListened b)
      create = do
        el <- getEventListened e
        outVertex <- newVertex "map" 0 []
        sourcesRefs <- traverse {t=List} newRef [newSource (vertex el) (listen_ e outVertex (\v => send_ out (f v)) False)]  -- TODO: lambda1 deps
        modifyRef outVertex (record{sources = sourcesRefs})
        newEventListened outVertex

      eventListenedRef : IORef (GetEventListened b)
      eventListenedRef = unsafeNewIORef (CreateEventListened create) e

export
main : IO ()
main = do runStateT r ini
          pure ()
  where r : Reactive ()
        r = do
          e <- newEvent {a=Integer}
          let e' = map (+ 1) e
          send_ e 24
        ini = MkReactiveState False [] [] [] 0
