module FRP.Sodium

import Data.IORef

ID : Type
ID = Int

Reactive : Type -> Type
Reactive = IO

requestRegen : Reactive ()

scheduleLast : Reactive () -> Reactive ()

VertexID : Type
VertexID = ID

record Vertex where
  constructor MkVertex
  name     : String
  vertexID : VertexID
  rank     : Int

increment : IORef Vertex -> IORef Vertex -> IO Bool

register : IORef Vertex -> IORef Vertex -> IO Bool
register = increment

deregister : IORef Vertex -> IORef Vertex -> IO ()

record Listener a where
  constructor MkListener
  handler : (a -> Reactive ())
  targetID  : VertexID

Eq (Listener a) where

record Event a where
  constructor MkEvent
  -- purify it, most likely similarily to Sodium Haskell
  vertex : IORef Vertex
  listeners : IORef (List (Listener a))
  firings : IORef (List a)

listen_ : Event a -> IORef Vertex -> (a -> Reactive ()) -> Bool -> Reactive (IO ())
listen_ ev target h suppressEarlierFirings = do
    when !(register (vertex ev) target) requestRegen
    let listener = MkListener h !(map vertexID $ readIORef target)
    modifyIORef (listeners ev) (listener::)
    firings <- readIORef (firings ev)
    when (not suppressEarlierFirings) $ traverse_ h firings -- prioritized?
    pure $ unlisten ev listener
  where
    unlisten ev listener = do
      modifyIORef (listeners ev) (delete listener)
      deregister (vertex ev) target

send_ : Event a -> a -> Reactive ()
send_ ev v = do
 -- TODO: vertex refCount check
 when (isNil !(readIORef (firings ev))) $ scheduleLast $
   modifyIORef (firings ev) (const [])
 modifyIORef (firings ev) (v::)
 traverse_ (\l => (handler l) v) !(readIORef (listeners ev))

map : (a -> b) -> Event a -> Event b
-- map f s =
--   let out = MkStream
