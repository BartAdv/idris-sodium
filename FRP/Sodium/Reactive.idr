module FRP.Sodium.Reactive

import public Control.Monad.State
import Data.IORef
import FRP.Sodium.Vertex

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
    nextID  : VertexID

  public export
  Reactive : Type -> Type
  Reactive a = StateT ReactiveState IO a

requestRegen : Reactive ()
requestRegen = modify (record {toRegen = True})

scheduleLast : Reactive () -> Reactive ()

newVertex : String -> Rank -> List (SourceRef) -> Reactive VertexRef
newVertex name rank sources = do
  vID <- gets nextID
  let vertex = MkVertex name vID rank sources [] [] False
  modify (record{nextID = vID + 1})
  pure !(lift $ newIORef vertex)
