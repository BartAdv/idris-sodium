module FRP.Sodium.Reactive

import public Control.Monad.State
import Data.IORef

%access export

mutual
  -- uses inefficient data structures for now
  record ReactiveState where
    constructor MkReactiveState
    toRegen : Bool
    queue1  : List (Reactive ())
    -- queue2  : PriorityQueue (Maybe (MVar Node)) (Reactive ())
    final   : List (Reactive ())
    post    : List (Reactive ())
    nextID  : Integer

  public export
  Reactive : Type -> Type
  Reactive a = StateT ReactiveState IO a

requestRegen : Reactive ()
requestRegen = modify (record {toRegen = True})

scheduleLast : Reactive () -> Reactive ()
scheduleLast task = modify (record {final $= (++ [task])})

newRef : a -> Reactive (IORef a)
newRef = lift . newIORef

readRef : IORef a -> Reactive a
readRef = lift . readIORef

modifyRef : IORef a -> (a -> a) -> Reactive ()
modifyRef ref f = lift $ modifyIORef ref f

writeRef : IORef a -> a -> Reactive ()
writeRef ref v = lift $ writeIORef ref v

uniqueID : Reactive Integer
uniqueID = do
  vID <- gets nextID
  modify (record{nextID = vID + 1})
  pure vID
