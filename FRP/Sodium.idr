module FRP.Sodium

import Data.IORef
import public FRP.Sodium.Reactive
import FRP.Sodium.Vertex
import public FRP.Sodium.Event

import Debug.Trace


export
test2 : IO ()
test2 = do
  ref <- newIORef 2
  writeIORef ref 4
  printLn !(readIORef ref)
