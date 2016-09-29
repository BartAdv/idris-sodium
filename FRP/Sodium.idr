module FRP.Sodium

import Data.IORef
import public FRP.Sodium.Reactive
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
-- still wondering if the name fits
record Subscription a where
  constructor MkSubscription
  vertex : VertexRef
  listeners : IORef (List (Listener a))
  firings : IORef (List a)

newSubscription : VertexRef -> Reactive (Subscription a)
newSubscription vertex = pure $ MkSubscription vertex !(newRef []) !(newRef [])

-- this helps to hide mutability
data ReactiveSetup a = Create (Reactive a)
                     | Cached a

GetSubscription : Type -> Type
GetSubscription a = ReactiveSetup (Subscription a)

export
record Event a where
  constructor MkEvent
  subscription : IORef (GetSubscription a)

mkEvent : (IORef (GetSubscription a) -> Reactive (Subscription a)) -> Event a
mkEvent create = MkEvent $ unsafeKnottedRef (Create . create)

printSubscription : Subscription a -> Reactive ()
printSubscription (MkSubscription vertex listeners firings) = do
  lift $ printLn $ "Subscription " ++ show !(readRef vertex) ++ ", firings: " ++ show (length !(readRef firings)) ++ ", listeners: " ++ show (length !(readRef listeners))

getSubscription_ : IORef (GetSubscription a) -> Reactive (Subscription a)
getSubscription_ subscriptionRef = do
  Create create <- readRef subscriptionRef | Cached el => pure el
  el <- create
  writeRef subscriptionRef (Cached el)
  pure el

getSubscription : Event a -> Reactive (Subscription a)
getSubscription (MkEvent subscriptionRef) = getSubscription_ subscriptionRef

export
printEvent : Event a -> Reactive ()
printEvent ev = do
  el <- getSubscription ev
  printSubscription el

listen_ : Event a -> VertexRef -> (a -> Reactive ()) -> Bool -> Register
listen_ ev target h suppressEarlierFirings = do
    MkSubscription vertex listeners firings <- getSubscription ev
    when !(registerVertex vertex target) requestRegen
    let listener = MkListener h (vertexID !(readRef target))
    modifyRef listeners (listener::)
    when (not suppressEarlierFirings) $ traverse_ h !(readRef firings) -- prioritized?
    pure $ unlisten ev listener
  where
    unlisten : Event a -> Listener a -> Deregister
    unlisten (MkEvent subscriptionRef) listener = do
      Cached (MkSubscription vertex listeners _) <- readRef subscriptionRef | Create _ => pure ()
      modifyRef listeners (delete listener)
      deregisterVertex vertex target

send_ : Subscription a -> a -> Reactive ()
send_ (MkSubscription _ listeners firings) v = do
  -- TODO: vertex refCount check
  when (isNil !(readRef firings)) $ scheduleLast $
    modifyRef firings (const [])
  modifyRef firings (v::)
  traverse_ (\l => (handler l) v) !(readRef listeners)

%access export

newEvent : Reactive (Event a)
newEvent = do
  vertex <- newVertex "Event" 0 []
  el <- newSubscription vertex
  let ev = MkEvent !(newRef (Cached el))
  pure ev

listen : Event a -> (a -> Reactive ()) -> Reactive Deregister
listen ev h = do
  vertex <- newVertex "user" 99999 [] -- TODO: maxBound?
  listen_ ev vertex h False

send : Event a -> a -> Reactive ()
send e v = send_ !(getSubscription e) v

map : (a -> b) -> Event a -> Event b
map f e = mkEvent create
  where
    create outRef = do
      el <- getSubscription e
      outVertex <- newVertex "map" 0 []
      let cb = \v => send_ !(getSubscription_ outRef) (f v)
      setSources outVertex [newSource (vertex el) (listen_ e outVertex cb False)]
      newSubscription outVertex

-- TODO: map with Lambda1/deps

private
merge_ : Event a -> Event a -> Event a
merge_ e1 e2 = mkEvent create
  where
    create outRef = do
      MkSubscription e1Vertex _ _ <- getSubscription e1
      MkSubscription e2Vertex _ _ <- getSubscription e2
      outVertex   <- newVertex "Merged event" 0 [] -- original was just default "Event" name
      leftVertex  <- newVertex "merge" 0 []
      leftSources <- traverse newRef [newSource e1Vertex (listen_ e1 leftVertex (\v => send_ !(getSubscription_ outRef) v) False)]
      outSources  <- traverse newRef [newSource leftVertex (do registerVertex leftVertex outVertex
                                                               pure $ deregisterVertex leftVertex outVertex),
                                      newSource e2Vertex (listen_ e2 outVertex (\v => send_ !(getSubscription_ outRef) v) False)]
      newSubscription outVertex

filter : (a -> Bool) -> Event a -> Event a
filter ff e = mkEvent create
  where
    create outRef = do
      el <- getSubscription e
      outVertex <- newVertex "filter" 0 []
      let cb = \v => when (ff v) $ send_ !(getSubscription_ outRef) v
      setSources outVertex [newSource (vertex el) (listen_ e outVertex cb False)]
      newSubscription outVertex

-- TODO: filter with Lambda1/deps

--
-- CELLS
--

%access private

-- the state for the apply primitive consist of a function and a value, both optional
ApplyState : Type -> Type -> Type
ApplyState a b = (Maybe (a -> b), Maybe a)

record Sample a where
  constructor MkSample
  value : a
  vertex : VertexRef
  valueUpdate : Maybe a

GetSample : Type -> Type
GetSample a = ReactiveSetup (Sample a)

export
record Cell a where
  constructor MkCell
  event : Event a
  sample : IORef (GetSample a)

-- similar to mkEvent, don't know if it's needed at all
mkCell : Event a -> (IORef (GetSample a) -> Reactive (Sample a)) -> Cell a
mkCell e create = MkCell e $ unsafeKnottedRef (Create . create)

getSample_ : IORef (GetSample a) -> Reactive (Sample a)
getSample_ ref = do
  Create create <- readRef ref | Cached el => pure el
  el <- create
  writeRef ref (Cached el)
  pure el

getSample : Cell a -> Reactive (Sample a)
getSample (MkCell _ ref) = getSample_ ref

newSample : a -> VertexRef -> Reactive (Sample a)
newSample v vertex = pure $ MkSample v vertex Nothing

%access export

hold : Event a -> a -> Reactive (Cell a)
hold e initValue = pure $
  mkCell e $ \outRef => do
    outVertex <- newVertex "Cell" 0 []
    newSample initValue outVertex
    -- TODO: setStream stuff

constCell : a -> Reactive (Cell a)
constCell initValue = do
  e <- newEvent
  hold e initValue

sample : Cell a -> Reactive a
sample c = do
  s <- getSample c
  pure $ value s

snapshot : Event a -> Cell b -> (a -> b -> c) -> Event c
snapshot e c f =
  mkEvent $ \outRef => do
    outVertex <- newVertex "snapshot" 0 []
    let cb = \v => send_ !(getSubscription_ outRef) (f v !(sample c))
    setSources outVertex [newSource (vertex !(getSubscription e)) (listen_ e outVertex cb False),
                          newSource (vertex !(getSample c)) noRegister]
    newSubscription outVertex

