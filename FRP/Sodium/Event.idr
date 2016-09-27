module FRP.Sodium.Event

import Data.IORef
import FRP.Sodium.Reactive
import FRP.Sodium.Vertex

%access private

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

export
record Event a where
  constructor MkEvent
  eventListened : IORef (GetEventListened a)

mkEvent : (IORef (GetEventListened a) -> Reactive (EventListened a)) -> Event a
mkEvent create = MkEvent $ unsafeKnottedRef (CreateEventListened . create)

printEventListened : EventListened a -> Reactive ()
printEventListened (MkEventListened vertex listeners firings) = do
  lift $ printLn $ "EventListened " ++ show !(readRef vertex) ++ ", firings: " ++ show (length !(readRef firings)) ++ ", listeners: " ++ show (length !(readRef listeners))

private
getEventListened_ : IORef (GetEventListened a) -> Reactive (EventListened a)
getEventListened_ eventListenedRef = do
  CreateEventListened create <- readRef eventListenedRef | CachedEventListened el => pure el
  el <- create
  writeRef eventListenedRef (CachedEventListened el)
  pure el

getEventListened : Event a -> Reactive (EventListened a)
getEventListened (MkEvent eventListenedRef) = getEventListened_ eventListenedRef

export
printEvent : Event a -> Reactive ()
printEvent ev = do
  el <- getEventListened ev
  printEventListened el

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

send_ : EventListened a -> a -> Reactive ()
send_ (MkEventListened _ listeners firings) v = do
  -- TODO: vertex refCount check
  when (isNil !(readRef firings)) $ scheduleLast $
    modifyRef firings (const [])
  modifyRef firings (v::)
  traverse_ (\l => (handler l) v) !(readRef listeners)

%access export

newEvent : Reactive (Event a)
newEvent = do
  vertex <- newVertex "Event" 0 []
  el <- newEventListened vertex
  let ev = MkEvent !(newRef (CachedEventListened el))
  pure ev

listen : Event a -> (a -> Reactive ()) -> Reactive Deregister
listen ev h = do
  vertex <- newVertex "user" 99999 [] -- TODO: maxBound?
  listen_ ev vertex h False

send : Event a -> a -> Reactive ()
send e v = send_ !(getEventListened e) v

map : (a -> b) -> Event a -> Event b
map f e = mkEvent create
  where
    create outRef = do
      el <- getEventListened e
      outVertex <- newVertex "map" 0 []
      let cb = \v => send_ !(getEventListened_ outRef) (f v)
      sourcesRefs <- traverse newRef [newSource (vertex el) (listen_ e outVertex cb False)]
      modifyRef outVertex (record{sources = sourcesRefs})
      newEventListened outVertex

-- TODO: map with Lambda1/deps

private
merge_ : Event a -> Event a -> Event a
merge_ e1 e2 = MkEvent $ unsafeKnottedRef create
  where
    create : IORef (GetEventListened a) -> GetEventListened a
    create outRef = CreateEventListened $ do
      MkEventListened e1Vertex _ _ <- getEventListened e1
      MkEventListened e2Vertex _ _ <- getEventListened e2
      outVertex   <- newVertex "Merged event" 0 [] -- original was just default "Stream" name
      leftVertex  <- newVertex "merge" 0 []
      leftSources <- traverse newRef [newSource e1Vertex (listen_ e1 leftVertex (\v => send_ !(getEventListened_ outRef) v) False)]
      outSources  <- traverse newRef [newSource leftVertex (do registerVertex leftVertex outVertex
                                                               pure $ deregisterVertex leftVertex outVertex),
                                      newSource e2Vertex (listen_ e2 outVertex (\v => send_ !(getEventListened_ outRef) v) False)]
      newEventListened outVertex

filter : (a -> Bool) -> Event a -> Event a
filter ff e = mkEvent create
  where
    create outRef = do
      MkEventListened vertex _ _ <- getEventListened e
      outVertex <- newVertex "filter" 0 []
      let cb = \v => when (ff v) $ send_ !(getEventListened_ outRef) v
      sources <- traverse newRef [newSource vertex (listen_ e outVertex cb False)]
      newEventListened outVertex

-- TODO: filter with Lambda1/deps
